library(shiny)

#set up ui object
QSIS21TextAnalytics_ui <- fluidPage(
  
  selectInput(inputId = "service",
              label = "Choose service",
              choices = unique(team_topics$service)),
  selectInput(inputId = "team",
              label = "Choose team",
              choices = unique(team_topics$team_name)),
  uiOutput("secondSelection"),
  plotOutput("topics_by_service"),
  plotOutput("team_topics_plot"),
  dataTableOutput("risk_comments")
)