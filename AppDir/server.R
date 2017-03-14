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
  
  
}

