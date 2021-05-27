library(shiny)
library(tidyverse)
library(stringr)
library(wordcloud2)
library(mgcv)
library(lubridate)

source("functions/get_topic_phi_and_gamma.R")

setwd("C:/Users/EJones1/Data Science Accelerator/QSIS21TextAnalytics")

#set k value
k_value = 40

model <- readRDS(paste0("data/model",k_value,".rds"))

qsis_self_declarations <- read.csv("data/qsis_self_declarations.csv")
team_info <- qsis_self_declarations %>%
  distinct(team_id,team_name,subservice_name,service_name, crg,poc) %>%
  mutate(service = str_replace(paste(service_name,subservice_name),"NA","")) #concatenate service & subservice then remove NA where no subservice

tidy_risk_comments <- read.csv("data/tidied_risk_comments.csv")

model_summary <- SummarizeTopics(model)

#get results from prediction and add back on metadata
prediction <- read.csv(paste0("data/prediction",k_value,".csv"))
prediction <- prediction %>% remove_rownames %>% column_to_rownames(var="X")
topiccy_words <- read.csv(paste0("data/long_topiccy_words",k_value,".csv"))
modelled_comments <- pivot_longer(prediction, -doc_id ,names_to = "topic", values_to = "probability") %>%
  group_by(doc_id) %>%
  arrange(-probability) %>%
  mutate(topic_n = row_number()) %>%
  ungroup() %>%
  left_join(tidy_risk_comments, by = c("doc_id"="doc")) %>%
  left_join(team_info, by = "team_id") %>%
  mutate(publish_on = dmy(publish_on),
         year = year(publish_on)) %>%
  left_join(topiccy_words, by = c("doc_id", "topic_n")) %>%
  ungroup()

topics_by_service <- modelled_comments %>%
  select(service,year,topic,probability) %>%
  group_by(service,year,topic) %>%
  mutate(probability = sum(probability)) %>%
  distinct()

topics_by_crg <- modelled_comments %>%
  select(crg,year,topic,probability) %>%
  group_by(crg,year,topic) %>%
  mutate(probability = sum(probability)) %>%
  distinct()

topics_by_poc <- modelled_comments %>%
  select(poc,year,topic,probability) %>%
  group_by(poc,year,topic) %>%
  mutate(probability = sum(probability)) %>%
  distinct()

topic_terms <- get_topic_phi_and_gamma(model)

#knit ui and server together in shiny app
runApp("shiny")