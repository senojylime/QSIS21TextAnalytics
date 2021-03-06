QSIS21TextAnalytics_server <- function(input, output, session) {
  
  observeEvent(input$poc,{
    updateSelectInput(session,'crg',
                      choices=unique(modelled_comments$crg[modelled_comments$poc==input$poc]))
  }) 


observeEvent(input$crg,{
  updateSelectInput(session,'service',
                    choices=unique(modelled_comments$service[modelled_comments$crg==input$crg]))
}) 

output$topics_by_poc <- renderPlot({
  
  modelled_comments %>%
    filter(poc == input$poc
    ) %>%
    select(topic,service,probability) %>%
    group_by(service,topic) %>%
    mutate(probability = sum(probability)) %>%
    distinct() %>%
    ungroup() %>%
    group_by(service) %>%
    slice_max(order_by = probability, n = 10) %>%
    ungroup() %>%
    distinct() %>%
    ggplot(aes(x=topic,y=service,fill=probability))+
    geom_tile() +
    scale_x_discrete(position = "top") +
    theme(axis.text.x = element_text(angle=90),
          legend.position = "none") +
    scale_fill_continuous(high = "#132B43", low = "#56B1F7")
  
})

output$topics_by_crg <- renderPlot({
  
  topics_by_crg %>%
    filter(crg == input$crg) %>%
    group_by(year) %>%
    slice_max(order_by = probability, n = 5) %>%
    ungroup() %>%
    left_join(model_summary, by = "topic") %>%
    ggplot(aes(x=reorder_within(topic,probability,year),
               y=probability,
               fill=topic)) +
    geom_col() +
    theme(legend.position = "none") +
    facet_wrap(~year, scales = "free_y") +
    geom_text(aes(y=0,x=reorder_within(topic,probability,year),label = top_terms_phi), hjust = "inward") +
    coord_flip() +
    scale_x_reordered() +
    labs(title = "Top topics by year",
         y = "Topic")
  
})

  output$topics_by_service <- renderPlot({
    
    topics_by_service %>%
      filter(service == input$service) %>%
      group_by(year) %>%
      slice_max(order_by = probability, n = 5) %>%
      ungroup() %>%
      left_join(model_summary, by = "topic") %>%
      ggplot(aes(x=reorder_within(topic,probability,year),
                 y=probability,
                 fill=topic)) +
      geom_col() +
      theme(legend.position = "none") +
      facet_wrap(~year, scales = "free_y") +
      geom_text(aes(y=0,x=reorder_within(topic,probability,year),label = top_terms_phi), hjust = "inward") +
      coord_flip() +
      scale_x_reordered() +
      labs(title = "Top topics by year",
           y = "Topic")
    
  })
  
  output$risk_comments <- renderDataTable({
    
    # top_topics <- topics_by_service %>%
    #   filter(service == input$service
    #   ) %>%
    #   slice_max(order_by = probability, n = 5) %>%
    #   distinct(topic)
    # top_topics <- top_topics[["topic"]] 
    
    modelled_comments %>%
      #group_by(doc_id) %>%
      #slice_max(order_by = probability,n = 5) %>%
      #ungroup() %>%
      filter(service == input$service
             #& topic %in% top_topics
             ) %>%
      select(team_name,year,#topic,
             pr_risk_comments) %>%
      arrange(team_name,year#,topic
              ) %>%
      group_by(team_name,year,pr_risk_comments) %>%
      #summarise(topics = toString(topic)) %>%
      distinct()
  })
  
  
  output$topic_wordcloud <- renderWordcloud2 ({
    
    topic_terms <- topic_terms %>%
      filter(topic == input$topic) %>%
      select(term,size)
  
  wordcloud2(data=topic_terms, size = 0.2)
  })
}