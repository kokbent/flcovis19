#RENDER COUNTY-LEVEL PLOTS
output$countyCases <- renderCachedPlot({
  if(input$smoothToggle == TRUE){
    countyPlot(input$county) +
      geom_line(aes(x = EventDate, y = ca7), lwd = 1.2)
  } 
  else {
    countyPlot(input$county)
  }
},
cacheKeyExpr = { list(input$county, input$smoothToggle) }
)



#RENDER COUNTY-LEVEL SELCECTED DATA
output$countyInfo <- renderDT({
  if(input$smoothToggle == TRUE){
    if(is.null(input$countyClick$x)){
      initDT <- split_counties[[input$county]][nrow(split_counties[[input$county]]),c(1,2,3,7)]
      initDT$EventDate <- initDT$EventDate %>% format(format = "%B %d, %Y")
      return(datatable(initDT, colnames = c("Event Date", "County", 
                                            "Number of Cases", "Centered Average")))
    }
    else if(is.null(input$countyBrush$xmin)) {
      lvls <- split_counties[[input$county]]$EventDate %>% 
        factor() %>% 
        levels() 
      dateTest <- lvls[round(input$countyClick$x-18261)]
      pointData <- split_counties[[input$county]][which(split_counties[[input$county]]$EventDate == dateTest), ]
      pointData$EventDate <- pointData$EventDate %>% format(format = "%B %d, %Y")
      return(datatable(pointData[,c(1,2,3,7)], colnames = c("Event Date", "County", 
                                                            "Number of Cases", "Centered Average")))
    }
    else{
      brushData <- brushedPoints(split_counties[[input$county]], input$countyBrush)
      brushData$EventDate <- brushData$EventDate %>% format(format = "%B %d, %Y")
      return(datatable(brushData[,c(1,2,3,7)], colnames = c("Event Date", "County", 
                                                            "Number of Cases", "Centered Average")))
    }
  }
  else{
    if(is.null(input$countyClick$x)){
      initDT <- split_counties[[input$county]][nrow(split_counties[[input$county]]),c(1,2,3)]
      initDT$EventDate <- initDT$EventDate %>% format(format = "%B %d, %Y")
      return(datatable(initDT, colnames = c("Event Date", "County", 
                                            "Number of Cases")))
    }
    else if(is.null(input$countyBrush$xmin)) {
      lvls <- split_counties[[input$county]]$EventDate %>% 
        factor() %>% 
        levels() 
      dateTest <- lvls[round(input$countyClick$x-18261)]
      pointData <- split_counties[[input$county]][which(split_counties[[input$county]]$EventDate == dateTest), ]
      pointData$EventDate <- pointData$EventDate %>% format(format = "%B %d, %Y")
      return(datatable(pointData[,c(1,2,3)], colnames = c("Event Date", "County", 
                                                          "Number of Cases")))
    }
    else{
      brushData <- brushedPoints(split_counties[[input$county]], input$countyBrush)
      brushData$EventDate <- brushData$EventDate %>% format(format = "%B %d, %Y")
      return(datatable(brushData[,c(1,2,3)], colnames = c("Event Date", "County", 
                                                          "Number of Cases")))
    }
  }
})