library(shiny)
library(tidyverse)
library(stringr)
library(wordcloud2)

setwd("C:/Users/EJones1/Data Science Accelerator/QSIS21TextAnalytics")
modelled_comments <- read.csv("data/modelled_comments25.csv")
modelled_comments <- modelled_comments %>% arrange(service,team_name)
topics_by_service <- read.csv("data/topics_by_service.csv")
topics_by_crg <- read.csv("data/topics_by_crg.csv")
topics_by_poc <- read.csv("data/topics_by_poc.csv")
model_summary <- read.csv("data/model_summary.csv")
model_summary <- model_summary %>% select(-X)
model <- load("data/model.RData") #this doesn't work, must have saved it wrong
topic_terms <- read.csv("data/topic_terms.csv")

#knit ui and server together in shiny app
runApp("shiny")