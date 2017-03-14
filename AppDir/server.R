function(input, output, session) {
  output$totaljobs <- renderValueBox({
    totalj <- bubbleData[,sum(value)]
    
    valueBox(
      value = totalj,
      subtitle = "Total job opportunities",
      icon = icon("id-card"),
      color = "light-blue"
    )
  })
  output$myMap <- renderLeaflet({
    leaflet(countr) %>%
      addTiles() %>%
      addPolygons(
                  fillOpacity = .2,
                  weight = 1,
                  stroke = T,
                  color = "#ff0000"
      )
  })
  output$totalseeker <- renderValueBox({
    valueBox(
      1e6,
      "Available labour force",
      icon = icon("users"),color="light-blue"
    )
  })
  
  output$groupjobs <- renderValueBox({
    
    valueBox(
      value = bubbleData[job_groups==input$jobgp,value],
      subtitle = input$jobgp,
      icon = icon("id-card"),
      color = "light-blue"
    )
  })
  
  output$groupseeker <- renderValueBox({
    valueBox(
      100,
      "Number of matched seekers",
      icon = icon("users"),color = "light-blue"
    )
  })
  
  output$distPlot <- renderPlotly({
    print(plotMain(bubbleData))
  }) 
  output$plotgroup <- renderPlot({
    print(plotBar(skillmiss,input$jobgp))
  }) 
  
  observeEvent(input$myMap_shape_click, {
    updateSelectInput(session, "Country",
                      selected = "Germany"
    )
    updateTabItems(session, "tabs", selected = "globalview")
  })
  
  output$plotnet <- renderPlot({
    plot(ggnet2(net[[input$skillg]],label=TRUE))
  }) 
}

