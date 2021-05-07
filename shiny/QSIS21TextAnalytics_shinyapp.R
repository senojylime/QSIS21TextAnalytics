library(shiny)
library(tidyverse)
library(stringr)
library(tidytext)
library(quanteda)
library(topicmodels)
library(textmineR)
library(lubridate)
library(ngram)

setwd("C:/Users/EJones1/Data Science Accelerator/QSIS21TextAnalytics")
modelled_comments <- read.csv("data/modelled_comments30.csv")
modelled_comments <- modelled_comments %>% arrange(service,team_name)
topics_by_service <- read.csv("data/topics_by_service.csv")
model_summary <- read.csv("data/model_summary.csv")
model_summary <- model_summary %>% select(-X)

#set up ui object
QSIS21TextAnalytics_ui <- fluidPage(
  
  titlePanel("QSIS Self-Declaration Risk Comments"),
  
  selectInput(inputId = "service",
              label = "Choose service",
              choices = unique(modelled_comments$service)),
  column(6, plotOutput("topics_by_service")),
  column(6, dataTableOutput("risk_comments"))
)

QSIS21TextAnalytics_server <- function(input, output, session) {
  
  output$topics_by_service <- renderPlot({
    
    topics_by_service %>%
      filter(service == input$service) %>%
      left_join(model_summary, by = "topic") %>%
      ggplot(aes(y=reorder(topic,probability),
                 x=probability,
                 fill=topic)) +
      geom_col() +
      theme(legend.position = "none") +
      facet_wrap(~year) +
      geom_text(aes(x=0,y=reorder(topic,probability),label = top_terms_gamma), hjust = "inward")
    
  })
  
  output$risk_comments <- renderDataTable({
    
    top_topics <- topics_by_service %>%
      filter(service == input$service
             ) %>%
      slice_max(order_by = probability, n = 5) %>%
      distinct(topic)
    top_topics <- top_topics[["topic"]]
      
    modelled_comments %>%
      group_by(doc_id) %>%
      slice_max(order_by = probability,n = 5) %>%
      ungroup() %>%
      filter(service == input$service
             & topic %in% top_topics) %>%
      select(team_name,year,topic,pr_risk_comments) %>%
      arrange(team_name,year,topic) %>%
      group_by(team_name,year,pr_risk_comments) %>%
      summarise(topics = toString(topic)) %>%
      distinct()
  })
  
}
#knit ui and server together in shiny app
shinyApp(ui = QSIS21TextAnalytics_ui, server = QSIS21TextAnalytics_server)