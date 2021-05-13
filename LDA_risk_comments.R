library(tidyverse)
library(stringr)
library(tidytext)
library(quanteda)
library(topicmodels)
library(textmineR)
library(lubridate)
library(ngram)

source("functions/get_most_toppiccy_words.R")

qsis_self_declarations <- read.csv("data/qsis_self_declarations.csv")

team_info <- qsis_self_declarations %>%
  distinct(team_id,team_name,subservice_name,service_name, crg,poc) %>%
  mutate(service = paste(service_name,subservice_name))

risk_docs <- qsis_self_declarations %>%
  distinct(service_name,team_id,publish_on,pr_risk_comments) %>%
  na.omit() %>%
  mutate(doc = paste(team_id,publish_on),
         word_count = sapply(strsplit(pr_risk_comments, " "), length))

#tidy the dataset, tokenise and remove general stop words
sd_tidy <-  risk_docs %>%
  unnest_tokens(word, pr_risk_comments, "words", drop = FALSE) %>%
  anti_join(stop_words)

# #Get a list of the most common words
common_words <- sd_tidy %>%
  count(word) %>%
  slice_max(order_by = n, n = 10) %>%
  filter(word != "mdt")

for_dtm <- sd_tidy %>%
  filter(word_count > 3 & !(word %in% c("quoracy","capacity","recruitment","theatre","palliative","locum","staff","transport","data","staffing",
                                        "attendance","itc","pathology","nurse","validation","lack"))) %>%
  filter(str_detect(pr_risk_comments, "No patient related risk|no risk|no known patient risk") == FALSE) %>%
  anti_join(common_words, by = "word") %>%
  distinct(team_id,publish_on,pr_risk_comments) %>%
  na.omit() %>%
  mutate(doc = paste(team_id,publish_on))

#create document term matrix
dtm <- CreateDtm(doc_vec = for_dtm$pr_risk_comments, # character vector of documents
                 doc_names = for_dtm$doc, # document names
                 ngram_window = c(1,2), # minimum and maximum n-gram length
                 stopword_vec = c(stopwords::stopwords("en"), # stopwords from tm
                                  stopwords::stopwords(source = "smart"),
                                  c("patient","patients","trusts","trust",
                                    "risk","risks","register","service","services","nhs","nhse","nhsei",
                                    "oncology","unit","north","south","east","west","cumbria","bristol","identified")), 
                 lower = TRUE, # lowercase - this is the default value
                 remove_punctuation = TRUE, # punctuation - this is the default
                 remove_numbers = TRUE, # numbers - this is the default
                 verbose = FALSE, # Turn off status bar for this demo
                 cpus = 2) # default is all available cpus on the system

dtm <- dtm[,colSums(dtm) > 2] #I can't remember what this is for?

#fit an LDA model
set.seed(12345)

model <- FitLdaModel(dtm = dtm, 
                     k = 30,
                     iterations = 500, # I usually recommend at least 500 iterations or more
                     burnin = 100,
                     alpha = 0.1,
                     beta = 0.05,
                     optimize_alpha = TRUE,
                     calc_likelihood = TRUE,
                     calc_coherence = TRUE,
                     calc_r2 = TRUE,
                     cpus = 2)

# predict topics contained in each document
prediction <- predict(model, dtm, method = "dot")
prediction <- as.data.frame(prediction)
prediction$doc_id <- rownames(prediction)

#get toppicy words from comment
topiccy_words <-  apply(dtm ,1, FUN = get_topiccy_words, model=model, top_n=3)

long_topiccy_words <- as.data.frame(topiccy_words) %>%
  mutate(topic_n = c(1,1,1,1,1,2,2,2,2,2,3,3,3,3,3)) %>%
  pivot_longer(!topic_n, names_to = "doc_id", values_to = "topiccy_words") %>%
  group_by(topic_n,doc_id) %>%
  summarise(topiccy_words = toString(topiccy_words))

#get results from prediction and add back on metadata
modelled_comments <- pivot_longer(prediction, -doc_id ,names_to = "topic", values_to = "probability") %>%
  group_by(doc_id) %>%
  arrange(-probability) %>%
  mutate(topic_n = row_number()) %>%
  ungroup() %>%
  left_join(risk_docs, by = c("doc_id"="doc")) %>%
  left_join(team_info, by = "team_id") %>%
  mutate(publish_on = dmy(publish_on),
         year = year(publish_on)) %>%
  select(-service_name.y) %>%
  rename("service_name" = service_name.x) %>%
  left_join(long_topiccy_words, by = c("doc_id", "topic_n")) %>%
  ungroup()
write.csv(modelled_comments,"data/modelled_comments30.csv")

#create summary of model
model_summary <- SummarizeTopics(model)
write.csv(model_summary, "data/model_summary.csv")

#topics by service
topics_by_service <- modelled_comments %>%
  select(service,year,topic,probability) %>%
  group_by(service,year,topic) %>%
  mutate(probability = sum(probability)) %>%
  distinct()
write.csv(topics_by_service, "data/topics_by_service.csv")
