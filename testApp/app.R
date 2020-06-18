library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)
library(lubridate)
library(DT)
library(shinyjs)
library(shinydashboard)
library(shinyWidgets)
library(cowplot)
library(plotly)

# IMPORT DATA INITIALIZATION FUNCTIONS
source("dataImportClean.R")
source("loadingMessages.R")
source("text.R")

appCSS <- "
#loading {
  position: absolute;
  background: #DCDCDC;
  opacity: 0.9;
  z-index: 100;
  left: 0;
  right: 0;
  height: 100%;
  text-align: center;
  color: #696969;
}
"

# DATA INITIALIZATION
getData() # Included dates stuff

# Codes and functions for plotting state and counties cases
source("plotsForStateCounties.R")

#INSERT AHCA CODE SECTION 1 HERE
# source("densityPlotApp.R")
# source("regDatPlots.R")

# DEFINE UI
ui <- tagList(
  useShinydashboard(),
  useShinyjs(),
  inlineCSS(appCSS),
  div(
    id = "loading",
    h1("FLOVID-19 DATA VISUALIZER 9000"),
    h2("LOADING"),
    h3(loadingMessages[sample(1:239, 1, replace = FALSE)])
  ),#div
  
  hidden(
    div(
      id = "hidden",
      navbarPage("FLovid-19 Data Visualizer 9000",
                 tabPanel("About",
                          fluidRow(
                            column(12, align = "center",
                                   box(width = NULL, status = "warning",
                                       h1("Hladish Lab's Data Visualization Hub for COVID-19"),
                                       h3("Otherwise known as the FLovid-19 Data Visualizer 9000")
                                   )#box
                            )#column
                          ),#fluidRow
                          fluidRow(
                            column(5, align = "center",
                                   box(width = NULL, status = "primary",
                                       h3("Hladish Lab Contributors"),
                                       p(aboutText)
                                   )#box
                            ),#column
                            column(7, align = "center",
                                   box(width = NULL, status = "primary",
                                       h3("About This Hub"),
                                       p(aboutText)
                                   )#box
                            )#column
                          )#fluidRow
                  ),#tabPanel
                 tabPanel("Statewide Cases",
                          fluidRow(
                            column(12, align = "center",
                                   h1("Number of Cases Statewide by Event Date"),
                                   h4("The shaded region represents data from the last 14 days and is likely to be revised"),
                                   plotOutput("stateCases", click = "stateClick", brush = brushOpts(id = "stateBrush", fill = "#ccc", direction = "x")),
                            )#column
                          ),#fluidRow
                          fluidRow(
                            column(1,
                                   dropdownButton(inputId = "stateDropdown", label = "Options", up = TRUE, icon = icon("gear"), status = "primary",
                                                  p("Toggle Smooth Line"),
                                                  switchInput(inputId = "stateSmoothToggle", value = TRUE)
                                    ),
                              ),
                            column(8, align = "center",
                                   box(title = "Selected Statewide Plot Data", width = NULL, status = "primary", solidHeader = TRUE,
                                       p("Click on a column in the plot to see more information for that day."),
                                       DT::DTOutput("stateInfo")
                                     )#box
                                   ),#column
                            column(3, align = "center",
                                   box(title = "Plot Information", width = NULL, status = "warning", solidHeader = TRUE,
                                     p(strong("Event Date: "), "definition."),
                                     p(strong("Number of Cases: "), "definition."),
                                     p(strong("Trailing Average: "), "definition."),
                                     p(strong("Centered Average: "), "definition.")
                                     )#box
                                   )#column
                          )#fluidRow
                 ),#tabPanel
                 tabPanel("County-Level Cases",
                          fluidRow(
                            column(12, align = "center",
                                   h1("Number of Cases in Selected County by Event Date"),
                                   h4("The shaded region represents data from the last 14 days and is likely to be revised"),
                                   h3("Select County"),
                                   selectInput("county", label = "", choices = names(split_counties)),
                                   plotOutput("countyCases", click = "countyClick", brush = brushOpts(id = "countyBrush", fill = "#ccc", direction = "x"))
                            )#column
                          ),#fluidRow
                          fluidRow(
                            column(1,
                                   dropdownButton(inputId = "countyDropdown", label = "Options", up = TRUE, icon = icon("gear"), status = "primary",
                                                  p("Toggle Smooth Line"),
                                                  switchInput(inputId = "countySmoothToggle", value = TRUE)
                                   ),
                            ),
                            column(8, align = "center",
                                   box(title = "Selcted County-Level Plot Data", width = NULL, status = "primary", solidHeader = TRUE,
                                       p("Click on a column in the plot to see more information for that day."),
                                       DT::DTOutput("countyInfo")
                                   )#box
                                  ),#column
                            column(3, align = "center",
                                   box(title = "Plot Information", width = NULL, status = "warning", solidHeader = TRUE,
                                       p(strong("Event Date: "), "definition."),
                                       p(strong("Number of Cases: "), "definition."),
                                       p(strong("Trailing Average: "), "definition."),
                                       p(strong("Centered Average: "), "definition.")
                                       )#box
                                   )#column
                          )#fluidRow
                 )#tabPanel - INSERT AHCA CODE SECTION 2 HERE
                 ## TODO - Smoothen this up
                 # tabPanel("Counties by Region",
                 #          fluidRow(
                 #            column(12, align = "center",
                 #                   h1("County Regions based on Adjusted Population Density"),
                 #                   plotOutput("densityPlot")
                 #            )
                 #          ),
                 #          fluidRow(
                 #            column(12, align = "center",
                 #                   h3("Select Region"),
                 #                   selectInput("region", label = "", choices = c("Region 1" = 1, "Region 2" = 2, "Region 3" = 3,
                 #                                                                 "Region 4" = 4, "Region 5" = 5, "Region 6" = 6)),
                 #                   plotOutput("regionPlot")
                 #            )
                 #          )
                 # )#tabPanel
      )#navbarPage
    )#div
  )#hidden
)#tagList

# DEFINE SERVER
server <- function(input, output, session) {
  
  # GATHER AND CLEAN DATA AFTER EVERY COUNTY SELECTION
  # setGlobalDates()
  # getData()
  # reDate()
  # caseCount()
  # dateAvgFill()
  
  
  
  # RENDER STATE PLOTS
  output$stateCases <- renderCachedPlot({
    if(input$stateSmoothToggle == TRUE){
      statePlot +
        geom_line(aes(x = EventDate, y = ma7_mean), data = pred_df, lwd = 1.1) +
        geom_ribbon(aes(x = EventDate, ymin = ma7_loCI, ymax = ma7_upCI), data = pred_df, alpha = 0.4)
    } 
    else {
      statePlot
    }
  },
  cacheKeyExpr = { list(input$stateSmoothToggle) }
  )
  
  #RENDER COUNTY-LEVEL PLOTS
  output$countyCases <- renderCachedPlot({
    if(input$countySmoothToggle == TRUE){
      countyPlot(input$county) +
        geom_line(aes(x = EventDate, y = ca7), lwd = 1.2)
    } 
    else {
      countyPlot(input$county)
    }
  },
  cacheKeyExpr = { list(input$county, input$countySmoothToggle) }
  )
  
  #RENDER STATE PLOT SELECTED DATA
  output$stateInfo <- renderDT({
    if(input$stateSmoothToggle == TRUE){
      if(is.null(input$stateClick$x)) {
        initDT <- case_ev[nrow(case_ev),c(1,2,6)]
        initDT$EventDate <- initDT$EventDate %>% format(format = "%B %d, %Y")
        return(datatable(initDT, colnames = c("Event Date", "Number of Cases", "Centered Average")))
      }
      else if(is.null(input$stateBrush$xmin)) { 
        lvls <- case_ev$EventDate %>% 
          factor() %>% 
          levels() 
        dateTest <- lvls[round((input$stateClick$x)-18261)]
        pointData <- case_ev[which(case_ev$EventDate == dateTest), ]
        pointData$EventDate <- pointData$EventDate %>% format(format = "%B %d, %Y")
        return(state_dt <- datatable(pointData[,c(1,2,6)], colnames = c("Event Date", "Number of Cases", "Centered Average")))
      }
      else{
        brushData <- brushedPoints(case_ev, input$stateBrush)
        brushData$EventDate <- brushData$EventDate %>% format(format = "%B %d, %Y")
        return(state_dt <- datatable(brushData[,c(1,2,6)], colnames = c("Event Date", "Number of Cases", "Centered Average")))
      }
    }
    else{
      if(is.null(input$stateClick$x)) {
        initDT <- case_ev[nrow(case_ev),c(1,2)]
        initDT$EventDate <- initDT$EventDate %>% format(format = "%B %d, %Y")
        return(datatable(initDT, colnames = c("Event Date", "Number of Cases")))
      }
      else if(is.null(input$stateBrush$xmin)) { 
        lvls <- case_ev$EventDate %>% 
          factor() %>% 
          levels() 
        dateTest <- lvls[round((input$stateClick$x)-18261)]
        pointData <- case_ev[which(case_ev$EventDate == dateTest), ]
        pointData$EventDate <- pointData$EventDate %>% format(format = "%B %d, %Y")
        return(state_dt <- datatable(pointData[,c(1,2)], colnames = c("Event Date", "Number of Cases")))
      }
      else{
        brushData <- brushedPoints(case_ev, input$stateBrush)
        brushData$EventDate <- brushData$EventDate %>% format(format = "%B %d, %Y")
        return(state_dt <- datatable(brushData[,c(1,2)], colnames = c("Event Date", "Number of Cases")))
      }
    }
  })
  
  #RENDER COUNTY-LEVEL SELCECTED DATA
  output$countyInfo <- renderDT({
    if(input$countySmoothToggle == TRUE){
      if(is.null(input$countyClick$x)){
        initDT <- split_counties[[input$county]][nrow(split_counties[[input$county]]),c(1,2,3,7)]
        initDT$EventDate <- initDT$EventDate %>% format(format = "%B %d, %Y")
        return(datatable(initDT, colnames = c("Event Date", "County", "Number of Cases", "Centered Average")))
      }
      else if(is.null(input$countyBrush$xmin)) {
        lvls <- split_counties[[input$county]]$EventDate %>% 
          factor() %>% 
          levels() 
        dateTest <- lvls[round(input$countyClick$x-18261)]
        pointData <- split_counties[[input$county]][which(split_counties[[input$county]]$EventDate == dateTest), ]
        pointData$EventDate <- pointData$EventDate %>% format(format = "%B %d, %Y")
        county_dt <- datatable(pointData[,c(1,2,3,7)], colnames = c("Event Date", "County", "Number of Cases", "Centered Average"))
      }
      else{
        brushData <- brushedPoints(split_counties[[input$county]], input$countyBrush)
        brushData$EventDate <- brushData$EventDate %>% format(format = "%B %d, %Y")
        return(state_dt <- datatable(brushData[,c(1,2,3,7)], colnames = c("Event Date", "County", "Number of Cases", "Centered Average")))
      }
    }
    else{
      if(is.null(input$countyClick$x)){
        initDT <- split_counties[[input$county]][nrow(split_counties[[input$county]]),c(1,2,3)]
        initDT$EventDate <- initDT$EventDate %>% format(format = "%B %d, %Y")
        return(datatable(initDT, colnames = c("Event Date", "County", "Number of Cases")))
      }
      else if(is.null(input$countyBrush$xmin)) {
        lvls <- split_counties[[input$county]]$EventDate %>% 
          factor() %>% 
          levels() 
        dateTest <- lvls[round(input$countyClick$x-18261)]
        pointData <- split_counties[[input$county]][which(split_counties[[input$county]]$EventDate == dateTest), ]
        pointData$EventDate <- pointData$EventDate %>% format(format = "%B %d, %Y")
        county_dt <- datatable(pointData[,c(1,2,3)], colnames = c("Event Date", "County", "Number of Cases"))
      }
      else{
        brushData <- brushedPoints(split_counties[[input$county]], input$countyBrush)
        brushData$EventDate <- brushData$EventDate %>% format(format = "%B %d, %Y")
        return(state_dt <- datatable(brushData[,c(1,2,3)], colnames = c("Event Date", "County", "Number of Cases")))
      }
    }
  })
  
  # output$densityPlot <- renderPlot({
  #   flPopDenPlot
  # })
  
  # output$regionPlot <- renderCachedPlot({
  #   reg_Plot(as.numeric(input$region))
  # },
  # cacheKeyExpr = { input$region }
  # )
  
  #INSERT AHCA CODE SECTION 3 HERE
  
  Sys.sleep(3)
  hide("loading", anim = TRUE, animType = "fade")
  show("hidden")
} 

# RUN APPLICATION
shinyApp(ui = ui, server = server)
