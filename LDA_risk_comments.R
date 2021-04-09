
library(tidyverse)
library(stringr)
library(tidytext)
library(quanteda)
library(topicmodels)


##--import datasets
sd_outcomes_report <- read.csv("sd_outcomes_report.csv") #QSIS front end report = general and risk comments
sd_data_export <- read.csv("sd_data_export.csv") #QSIS data export = indicator comments

##--tidy format
sd_outcomes_report <- sd_outcomes_report %>%
  group_by(Service, Subservice, Trust, Site, Period) %>%
  mutate(id = cur_group_id()) %>%
  rename("poc" = Programme.of.Care..POC.,
         "crg" = Clinical.Reference.Group..CRG.) %>%
  ungroup()

sd_outcomes_tidy <- sd_outcomes_report %>%
  select(id, Period, Risk.Comments) %>%
  separate(Period, into = c("FY", "description"), sep = " - ", extra = "merge") %>%
  unnest_tokens(output = token, input = Risk.Comments, token = "words")

common_words <- sd_outcomes_tidy %>%
  anti_join(stop_words, by = c("token"="word")) %>%
  group_by(FY) %>%
  count(token, sort = TRUE) %>%
  slice_max(order_by = n, n = 10)

##--DocumentTermMatrix
# sd_sparse <- sd_outcomes_tidy %>%
#   cast_sparse(row = group_id, column = token)

sd_dtm <- sd_outcomes_tidy %>%
  anti_join(stop_words, by = c("token"="word")) %>%
  filter(str_detect(token, "div|li|br|nbsp|?") != TRUE) %>%
  count(id, token) %>%
  cast_dtm(document = id, term = token, n)

#dfm quanteda package
# sd_dfm <- sd_outcomes_tidy %>%
#   count(group_id, token) %>%
#   cast_dfm(document = group_id, term = token, n)

##--LDA
# set a seed so that the output of the model is repeatable
sd_lda <- LDA(sd_dtm, k = 10, control = list(seed = 1234))

#Look at the beta values of words
#beta is the probability that a word will appear in that topic
sd_topics <- tidy(sd_lda, matrix = "beta")

sd_top_terms <- sd_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 5) %>% 
  ungroup() %>%
  arrange(topic, -beta)

sd_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

#terms that had the greatest difference in ?? between topics
#Not sure how to do this when there's more than 2 topics
# beta_wide <- sd_topics %>%
#   mutate(topic = paste0("topic", topic)) %>%
#   pivot_wider(names_from = topic, values_from = beta) %>% 
#   filter(topic1 > .001 | topic2 > .001) %>%
#   mutate(log_ratio = log2(topic2 / topic1))
# rbind(slice_max(beta_wide, log_ratio, n = 20), slice_min(beta_wide, log_ratio, n = 10)) %>%
#   ggplot(aes(y = reorder(term, log_ratio), x = log_ratio)) +
#   geom_col(show.legend = FALSE)

# gamma is the percentage of words in a document per topic
sd_gamma_topic <- tidy(sd_lda, matrix = "gamma") %>%
  group_by(document) %>%
  filter(gamma == max(gamma)) %>% #keep row with topic with highest gamma
  ungroup() %>%
  mutate(document = as.integer(document)) %>%
  left_join(sd_outcomes_report, by = c("document"="id")) %>% #join back to dataset with metadata
  select(-Commissioning, -Specialised.Commissioned.Type, -Team.Activity.Status, -Region, -Hub.Sub.Region,
         -Independent.Provider,-Status,-Status.Description,-Submitted.by,-Submitted.at,-Approved.by,-Approved.at) %>%
  mutate(topic = as.character(topic))

write_csv(sd_gamma_topic, "sd_gamma_topic.csv")

sd_gamma_topic %>%
  ggplot(aes(y = crg, fill = topic)) + #change x to poc, crg, Service etc
  geom_bar(position = "fill") +
  labs(y = "Proportion") +
  facet_wrap(~poc, scales = "free_y")

#assignments - using dtm instead of dfm now
assignments <- augment(sd_lda, data = sd_dtm)
