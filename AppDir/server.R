function(input, output, session) {
  output$totaljobs <- renderValueBox({
    totalj <- 1000
    
    valueBox(
      value = totalj,
      subtitle = "Total Job Advertisments",
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
      "Available Labour Force",
      icon = icon("users"),color="light-blue"
    )
  })
  
  output$groupjobs <- renderValueBox({
    
    valueBox(
      value = groupjobs[job_groups==input$jobgp,N],
      subtitle = input$jobgp,
      icon = icon("id-card"),
      color = "light-blue"
    )
  })
  
  output$groupseeker <- renderValueBox({
    valueBox(
      100,
      "Number of Matched seekers",
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

