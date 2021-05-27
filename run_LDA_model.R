library(tidyverse)
library(stringr)
library(tidytext)
library(quanteda)
library(topicmodels)
library(textmineR)
library(lubridate)
library(ngram)
library(maps)
library(mgcv)

setwd("C:/Users/EJones1/Data Science Accelerator/QSIS21TextAnalytics")

source("functions/get_most_toppiccy_words.R")
source("functions/tidy_the_data.R")

qsis_self_declarations <- read.csv("data/qsis_self_declarations.csv")

tidy_risk_comments <- tidy_the_data(qsis_self_declarations)
write.csv(tidy_risk_comments,"data/tidied_risk_comments_tokenised.csv")

for_dtm <- tidy_risk_comments %>%
  distinct(team_id,publish_on,pr_risk_comments,doc)
write.csv(for_dtm,"data/tidied_risk_comments.csv")

#set k
k_value = 40

#create document term matrix
dtm <- CreateDtm(doc_vec = for_dtm$pr_risk_comments, # character vector of documents
                 doc_names = for_dtm$doc, # document names
                 ngram_window = c(1,2), # minimum and maximum n-gram length
                 stopword_vec = c(stopwords::stopwords("en"), # stopwords from tm
                                  stopwords::stopwords(source = "smart"),
                                  c("patient","patients","trusts","trust",
                                    "risk","risks","register","service","services","nhs","nhse","nhsei",
                                    "oncology","unit","north","south","east","west","cumbria","bristol","identified"),
                                  world.cities$name), 
                 lower = TRUE, # lowercase - this is the default value
                 remove_punctuation = TRUE, # punctuation - this is the default
                 remove_numbers = TRUE, # numbers - this is the default
                 verbose = FALSE, # Turn off status bar for this demo
                 cpus = 2) # default is all available cpus on the system

dtm <- dtm[,colSums(dtm) > 2] #I can't remember what this is for?

#fit an LDA model
set.seed(12345)

model <- FitLdaModel(dtm = dtm, 
                     k = k_value,
                     iterations = 500, # I usually recommend at least 500 iterations or more
                     burnin = 100,
                     alpha = 0.1,
                     beta = 0.05,
                     optimize_alpha = TRUE,
                     calc_likelihood = TRUE,
                     calc_coherence = TRUE,
                     calc_r2 = TRUE,
                     cpus = 2)
saveRDS(model, file = paste0("data/model",k_value,".rds"))

# predict topics contained in each document
prediction <- predict(model, dtm, method = "dot")
prediction <- as.data.frame(prediction)
prediction$doc_id <- rownames(prediction)
write.csv(prediction,paste0("data/prediction",k_value,".csv"))

#get toppicy words from comment
topiccy_words <-  apply(dtm ,1, FUN = get_topiccy_words, model=model, top_n=3)
long_topiccy_words <- as.data.frame(topiccy_words) %>%
  mutate(topic_n = c(1,1,1,1,1,2,2,2,2,2,3,3,3,3,3)) %>%
  pivot_longer(!topic_n, names_to = "doc_id", values_to = "topiccy_words") %>%
  group_by(topic_n,doc_id) %>%
  summarise(topiccy_words = toString(topiccy_words))
write.csv(long_topiccy_words,paste0("data/long_topiccy_words",k_value,".csv"))




