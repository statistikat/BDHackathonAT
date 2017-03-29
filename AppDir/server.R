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
      round(npersav,-5),
      "Available persons",
      icon = icon("users"),color="light-blue"
    )
  })
  
  output$groupjobs <- renderValueBox({
    
    valueBox(
      value = sum(bubbleData[job_groups==input$jobgp,value]),
      subtitle = "Number of non-filled vaccancies",
      icon = icon("id-card"),
      color = "light-blue"
    )
  })
  
  output$groupseeker <- renderValueBox({
    valueBox(
      npersav-matched[,sum(N)],
      "Number of unmatched seekers",
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

