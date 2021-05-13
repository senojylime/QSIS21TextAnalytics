#set up ui object
QSIS21TextAnalytics_ui <- fluidPage(
  
  titlePanel("QSIS Self-Declaration Risk Comments"),
  
  selectInput(inputId = "poc",
              label = "Choose Programme of Care",
              choices = unique(modelled_comments$poc)),
  selectInput(inputId = "crg",
              label = "Choose Clinical Reference Group",
              choices = unique(modelled_comments$crg)),
  uiOutput("secondSelection"),
  selectInput(inputId = "service",
              label = "Choose Service",
              choices = unique(modelled_comments$service)),
  uiOutput("thirdSelection"),
  selectInput(inputId = "topic",
              label = "Choose Topic (for wordcloud only)",
              choices = unique(modelled_comments$topic)),
  mainPanel(
    tabsetPanel(
      tabPanel("PoC: Topic Heatmap", plotOutput("topics_by_poc")),
      tabPanel("CRG Top Topics", plotOutput("topics_by_crg")),
      tabPanel("Service: Top Topics by year",plotOutput("topics_by_service")),
      tabPanel("Service Comments", dataTableOutput("risk_comments")),
      tabPanel("Explore Topics", wordcloud2Output("topic_wordcloud"))
  )
  )
)