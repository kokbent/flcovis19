# RENDER STATE PLOTS
output$stateCases <- renderCachedPlot({
  if(input$smoothToggle == TRUE){
    statePlot +
      geom_line(aes(x = EventDate, y = ma7_mean), data = pred_df, lwd = 1.1) +
      geom_ribbon(aes(x = EventDate, ymin = ma7_loCI, ymax = ma7_upCI), data = pred_df, alpha = 0.4)
  } 
  else {
    statePlot
  }
},
cacheKeyExpr = { list(input$smoothToggle) }
)

# RENDER STATE PLOT SELECTED DATA
output$stateInfo <- renderDT({
  if(input$smoothToggle == TRUE){
    if(is.null(input$stateClick$x)) {
      initDT <- case_df[nrow(case_df),c(1,2,3,4)]
      initDT$EventDate <- initDT$EventDate %>% format(format = "%B %d, %Y")
      return(datatable(initDT, colnames = c("Event Date", "Reported Cases", 
                                            "Anticipated Cases", "Centered Average")))
    }
    else if(is.null(input$stateBrush$xmin)) { 
      lvls <- case_df$EventDate %>% 
        factor() %>% 
        levels() 
      dateTest <- lvls[round((input$stateClick$x)-18321)]
      pointData <- case_df[which(case_df$EventDate == dateTest), ]
      pointData$EventDate <- pointData$EventDate %>% format(format = "%B %d, %Y")
      return(datatable(pointData[,c(1,2,3,4)], 
                       colnames = c("Event Date", "Reported Cases", 
                                    "Anticipated Cases", "Centered Average")))
    }
    else{
      brushData <- brushedPoints(case_df, input$stateBrush)
      brushData$EventDate <- brushData$EventDate %>% format(format = "%B %d, %Y")
      return(datatable(brushData[,c(1,2,3,4)], 
                       colnames = c("Event Date", "Reported Cases", 
                                    "Anticipated Cases", "Centered Average")))
    }
  }
  else{
    if(is.null(input$stateClick$x)) {
      initDT <- case_df[nrow(case_df),c(1,2,3)]
      initDT$EventDate <- initDT$EventDate %>% format(format = "%B %d, %Y")
      return(datatable(initDT, 
                       colnames = c("Event Date", "Reported Cases", 
                                    "Anticipated Cases",)))
    }
    else if(is.null(input$stateBrush$xmin)) { 
      lvls <- case_df$EventDate %>% 
        factor() %>% 
        levels() 
      dateTest <- lvls[round((input$stateClick$x)-18261)]
      pointData <- case_df[which(case_df$EventDate == dateTest), ]
      pointData$EventDate <- pointData$EventDate %>% format(format = "%B %d, %Y")
      return(state_dt <- datatable(pointData[,c(1,2,3)], 
                                   colnames = c("Event Date", "Reported Cases", 
                                                "Anticipated Cases",)))
    }
    else{
      brushData <- brushedPoints(case_df, input$stateBrush)
      brushData$EventDate <- brushData$EventDate %>% format(format = "%B %d, %Y")
      return(state_dt <- datatable(brushData[,c(1,2,3)], 
                                   colnames = c("Event Date", "Reported Cases", 
                                                "Anticipated Cases",)))
    }
  }
})