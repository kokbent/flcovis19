#RENDER COUNTY-LEVEL PLOTS
# output$countyCases <- renderCachedPlot({
#   if(input$smoothToggle == TRUE){
#     countyPlot(input$county) +
#       geom_line(aes(x = EventDate, y = ca7), lwd = 1.2)
#   } 
#   else {
#     countyPlot(input$county)
#   }
# },
# cacheKeyExpr = { list(input$county, input$smoothToggle) }
# )

output$countyCases <- renderPlotly({
  countyData %>% 
    filter(County == input$county) %>% 
    plot_ly() %>%
    add_bars(x = ~EventDate,
             y = ~n,
             color = ~as.factor(weekend),
             colors = c("#0072B2", "#56b4e9"),
             name = ~ifelse(weekend, "Weekend Reported", "Weekday, "),
             text = ~paste('<b>', EventDate, '</b></br>', 
                           '</br>Reported cases (to date): ', n),
             hoverinfo = "text") %>%
    add_lines(x = ~EventDate,
              y = ~ca7,
              line = list(color = "black"),
              name = '7-day moving average (centered)',
              hoverinfo = "none",
              visible = "legendonly") %>%
    layout(
      legend = list(orientation = "h",
                    x = 0.5, y = 1,
                    xanchor = "center"),
      
      xaxis = list(
        title = "",
        rangeselector = list(
          buttons = list(
            list(
              count = 1,
              label = "1 mo",
              step = "month",
              stepmode = "backward"),
            list(
              count = 2,
              label = "2 mo",
              step = "month",
              stepmode = "backward"),
            list(step = "all"))),
        
        rangeslider = list(type = "date")),
      
      yaxis = list(title = "Reported Case"),
      
      barmode = "overlay",
      
      hovermode = "compare"
    ) %>%
    plotly_conf()
})



#RENDER COUNTY-LEVEL SELCECTED DATA
output$countyInfo <- renderDT({
  if(input$smoothToggle == TRUE){
    if(is.null(input$countyClick$x)){
      initDT <- split_counties[[input$county]][nrow(split_counties[[input$county]]),c(1,2,3,7)]
      initDT$ca7 <- initDT$ca7 %>% round(digits = 1)
      initDT$EventDate <- initDT$EventDate %>% format(format = "%B %d, %Y")
      return(datatable(initDT, colnames = c("Event date", "County", 
                                            "Reported cases", "Centered average")))
    }
    else if(is.null(input$countyBrush$xmin)) {
      lvls <- split_counties[[input$county]]$EventDate %>% 
        factor() %>% 
        levels() 
      dateTest <- lvls[round(input$countyClick$x-18261)]
      pointData <- split_counties[[input$county]][which(split_counties[[input$county]]$EventDate == dateTest), ]
      pointData$EventDate <- pointData$EventDate %>% format(format = "%B %d, %Y")
      pointData$ca7 <- pointData$ca7 %>% round(digits = 1)
      return(datatable(pointData[,c(1,2,3,7)], colnames = c("Event date", "County", 
                                                            "Reported cases", "Centered average")))
    }
    else{
      brushData <- brushedPoints(split_counties[[input$county]], input$countyBrush)
      brushData$EventDate <- brushData$EventDate %>% format(format = "%B %d, %Y")
      brushData$ca7 <- brushData$ca7 %>% round(digits = 1)
      return(datatable(brushData[,c(1,2,3,7)], colnames = c("Event date", "County", 
                                                            "Reported cases", "Centered average")))
    }
  }
  else{
    if(is.null(input$countyClick$x)){
      initDT <- split_counties[[input$county]][nrow(split_counties[[input$county]]),c(1,2,3)]
      initDT$EventDate <- initDT$EventDate %>% format(format = "%B %d, %Y")
      initDT$ca7 <- initDT$ca7 %>% round(digits = 1)
      return(datatable(initDT, colnames = c("Event date", "County", 
                                            "Reported cases")))
    }
    else if(is.null(input$countyBrush$xmin)) {
      lvls <- split_counties[[input$county]]$EventDate %>% 
        factor() %>% 
        levels() 
      dateTest <- lvls[round(input$countyClick$x-18261)]
      pointData <- split_counties[[input$county]][which(split_counties[[input$county]]$EventDate == dateTest), ]
      pointData$EventDate <- pointData$EventDate %>% format(format = "%B %d, %Y")
      pointData$ca7 <- pointData$ca7 %>% round(digits = 1)
      return(datatable(pointData[,c(1,2,3)], colnames = c("Event date", "County", 
                                                          "Reported cases")))
    }
    else{
      brushData <- brushedPoints(split_counties[[input$county]], input$countyBrush)
      brushData$EventDate <- brushData$EventDate %>% format(format = "%B %d, %Y")
      brushData$ca7 <- brushData$ca7 %>% round(digits = 1)
      return(datatable(brushData[,c(1,2,3)], colnames = c("Event date", "County", 
                                                          "Reported cases")))
    }
  }
})