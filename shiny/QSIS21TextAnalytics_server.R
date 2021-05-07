library(shiny)

QSIS21TextAnalytics_server <- function(input, output, session) {
  
  observeEvent(input$service,{
    updateSelectInput(session,'team',
                      choices=unique(team_topics$team_name[team_topics$service==input$service]))
  }) 
  
  output$topics_by_service <- renderPlot({
    
    topic_count_by_service %>%
      filter(service == input$service) %>%
      ggplot(aes(y=reorder(topic,n),
                 x=n,
                 fill=topic)) +
      geom_col() +
      theme(legend.position = "none") +
      facet_wrap(~year)
    
  })
  
  output$team_topics_plot <- renderPlot({
    
    for_plotting <- team_topics %>%
      filter(service == input$service
             & team_name == input$team)
    
    ggplot(for_plotting,
           aes(y=reorder(topic,topic_proportion),
               x=topic_proportion,fill=topic)) +
      geom_col() +
      theme(legend.position = "none") +
      facet_wrap(~year) +
      geom_text(aes(x=0,y=reorder(topic,topic_proportion),label = topiccy_words), hjust = "inward")
    
  })
  
  output$risk_comments <- renderDataTable({
    
    team_topics %>%
      filter(service == input$service
             & team_name == input$team) %>%
      select(year,pr_risk_comments) %>%
      arrange(year) %>%
      distinct()
  })
  
}