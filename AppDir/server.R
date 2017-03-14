function(input, output, session) {
  output$totaljobs <- renderValueBox({
    totalj <- 1000
    
    valueBox(
      value = totalj,
      subtitle = "Total Job Advertisments",
      icon = icon("id-card"),
      color = "aqua"
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
      icon = icon("users")
    )
  })
  
  output$groupjobs <- renderValueBox({
    
    valueBox(
      value = groupjobs[job_groups==input$jobgp,N],
      subtitle = input$jobgp,
      icon = icon("id-card"),
      color = "aqua"
    )
  })
  
  output$groupseeker <- renderValueBox({
    valueBox(
      100,
      "Number of Matched seekers",
      icon = icon("users")
    )
  })
  
  output$distPlot <- renderPlot({
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
    netx <- net[[input$skillg]]
    plot(ggnet2(netx,label=TRUE))
  }) 
}

