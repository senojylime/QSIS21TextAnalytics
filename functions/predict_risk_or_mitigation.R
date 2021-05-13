library(tidyverse)
library(stringr)
library(tidytext)
library(quanteda)
library(topicmodels)
library(textmineR)
library(lubridate)
library(ngram)
library(rsample)

qsis_self_declarations <- read.csv("C:/Users/EJones1/Data Science Accelerator/qsis_self_declarations.csv")

team_info <- qsis_self_declarations %>%
  distinct(team_id,team_name,subservice_name,service_name, crg,poc) %>%
  mutate(service = paste(service_name,subservice_name))

risk_docs <- qsis_self_declarations %>%
  distinct(service_name,team_id,publish_on,pr_risk_comments) %>%
  na.omit()

#tidy the dataset, tokenise
sd_tidy <-  risk_docs %>%
  unnest_tokens(sentence, pr_risk_comments, "sentences", drop = FALSE) %>%
  mutate(document = paste(team_id,publish_on,row_number()))

sd_tidy_classification <- read.csv("C:/Users/EJones1/Data Science Accelerator/QSIS21TextAnalytics/data/risk_sentences.csv")

#tokenise the dataset further
sd_tidy_word <- sd_tidy_classification %>%
  mutate(document_id = row_number()) %>%
  unnest_tokens(word, sentence,"words",drop = FALSE)

#split into training and testing datasets using rsample
# sd_split <- sd_tidy %>%
#   select(document) %>%
#   initial_split()

train_data <- sd_tidy_word %>% filter(classification != "")
#test_data <- testing(sd_split)

#Now we want to transform our training data from a tidy data structure to a sparse matrix to use for our machine learning algorithm.
sparse_words <- sd_tidy_word %>%
  count(document_id, word) %>%
  inner_join(train_data) %>%
  cast_sparse(document_id, word, n)

class(sparse_words)
dim(sparse_words)

#We also need to build a dataframe with a response variable to associate each of the rownames() of the sparse matrix with a title
#, to use as the quantity we will predict in the model.

word_rownames <- as.integer(rownames(sparse_words))

sd_joined <- data_frame(document_id = word_rownames) %>%
  left_join(sd_tidy_word %>%
              distinct(document_id, classification))
