
library(tidyverse)
library(stringr)
library(tidytext)
library(quanteda)
library(topicmodels)
library(textmineR)
library(lubridate)

#set working directory
setwd("C:/Users/EJones1/Data Science Accelerator")

#import self declaration dataset
qsis_self_declarations <- read.csv("qsis_self_declarations.csv")

risk_docs <- qsis_self_declarations %>%
  distinct(team_id,publish_on,pr_risk_comments) %>%
  na.omit() %>%
  mutate(doc = paste(team_id,publish_on))
  
#tidy the dataset, tokenise and remove general stop words
sd_tidy <-  risk_docs%>%
  unnest_tokens(word, pr_risk_comments, "words", drop = FALSE) %>%
  anti_join(stop_words)

#Get a list of the most common words
common_words <- sd_tidy %>%
  count(word) %>%
  slice_max(order_by = n, n = 10) %>%
  filter(word != "mdt")

#remove common words and domain stop words
sd_tidy <- sd_tidy %>%
  anti_join(common_words) %>%
  filter(str_detect(word,"patient|trusts|register|services|nhs*") == FALSE)

#### textmineR package
for_dtm <- qsis_self_declarations %>%
  distinct(team_id,publish_on,pr_risk_comments) %>%
  na.omit() %>%
  mutate(doc = paste(team_id,publish_on))

dtm <- CreateDtm(doc_vec = for_dtm$pr_risk_comments, # character vector of documents
                 doc_names = for_dtm$doc, # document names
                 ngram_window = c(1, 3), # minimum and maximum n-gram length
                 stopword_vec = c(stopwords::stopwords("en"), # stopwords from tm
                                  stopwords::stopwords(source = "smart"),
                                  common_words,
                                  c("patient","trusts","register","services","nhs","nhse","nhsei","nhsi")), # this is the default value
                 lower = TRUE, # lowercase - this is the default value
                 remove_punctuation = TRUE, # punctuation - this is the default
                 remove_numbers = TRUE, # numbers - this is the default
                 verbose = FALSE, # Turn off status bar for this demo
                 cpus = 2) # default is all available cpus on the system

dtm <- dtm[,colSums(dtm) > 2]

set.seed(12345)

model <- FitLdaModel(dtm = dtm, 
                     k = 75,
                     iterations = 500, # I usually recommend at least 500 iterations or more
                     burnin = 180,
                     alpha = 0.1,
                     beta = 0.05,
                     optimize_alpha = TRUE,
                     calc_likelihood = TRUE,
                     calc_coherence = TRUE,
                     calc_r2 = TRUE,
                     cpus = 2) 

# R-squared 
# - only works for probabilistic models like LDA and CTM
model$r2

# log Likelihood (does not consider the prior) 
plot(model$log_likelihood, type = "l")

summary(model$coherence)

hist(model$coherence, 
     col= "blue", 
     main = "Histogram of probabilistic coherence")

# Get the top terms of each topic
model$top_terms <- GetTopTerms(phi = model$phi, M = 5)

# Get the prevalence of each topic
# You can make this discrete by applying a threshold, say 0.05, for
# topics in/out of docuemnts. 
model$prevalence <- colSums(model$theta) / sum(model$theta) * 100

# prevalence should be proportional to alpha
plot(model$prevalence, model$alpha, xlab = "prevalence", ylab = "alpha")

# textmineR has a naive topic labeling tool based on probable bigrams
model$labels <- LabelTopics(assignments = model$theta > 0.05, 
                            dtm = dtm,
                            M = 1)

head(model$labels)

# put them together, with coherence into a summary table
model$summary <- data.frame(topic = rownames(model$phi),
                            label = model$labels,
                            coherence = round(model$coherence, 3),
                            prevalence = round(model$prevalence,3),
                            top_terms = apply(model$top_terms, 2, function(x){
                              paste(x, collapse = ", ")
                            }),
                            stringsAsFactors = FALSE)
model$summary[ order(model$summary$prevalence, decreasing = TRUE) , ][ 1:10 , ]

model_df <- as.data.frame(model$summary[ order(model$summary$prevalence, decreasing = TRUE) , ])

# predict on held-out documents using gibbs sampling "fold in"
gibbs_prediction <- predict(model, dtm, method = "gibbs",
              iterations = 200, burnin = 175)

# predict on held-out documents using the dot product method
dot_prediction <- predict(model, dtm, method = "dot")

# compare the methods
barplot(rbind(p1[1,],p2[1,]), beside = TRUE, col = c("red", "blue"))

gibbs_prediction <- as.data.frame(gibbs_prediction)
gibbs_prediction$doc_id <- rownames(gibbs_prediction) 
modelled_comments <- pivot_longer(gibbs_prediction, -doc_id ,names_to = "topic", values_to = "score") %>%
  left_join(model_df, by = "topic") %>%
  left_join(risk_docs, by = c("doc_id"="doc")) %>%
  group_by(doc_id) %>%
  slice_max(order_by = score, n = 3) %>%
  mutate(sum = sum(score),
         topic_proportion = score/sum*100,
         publish_on = dmy(publish_on),
         year = year(publish_on))
  
  

