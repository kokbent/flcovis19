library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)
library(lubridate)
library(DT)
library(sf)
library(shinydashboard)
library(shinyWidgets)
library(cowplot)
library(plotly)
library(leaflet)

# IMPORT DATA INITIALIZATION FUNCTIONS
source("functions.R")
source("text.R")

# DATA INITIALIZATION
getData() # Data from external repo (and local shapefile) plus some manipulation

# Codes and functions for plotting state and counties cases
source("plotsForStateCounties.R")

# GLOBAL OPTIONS
# TODO remove if DT no longer required
options(DT.options = list(searching = FALSE))

# DEFINE UI
ui <- dashboardPage(
  dashboardHeader(title = "Florida COVID-19 data visualizer",
                  titleWidth = 320),
  dashboardSidebar(
    width = 270,
    sidebarMenu(
      menuItem("Statewide cases", 
               tabName = "statewide", 
               icon = icon("dashboard"), 
               selected = T),
      menuItem("County cases", 
               tabName = "county", 
               icon = icon("dashboard")),
      menuItem("Statewide Death",
               tabName = "statewide-death",
               icon = icon("ambulance"),
               badgeLabel = "beta",
               badgeColor = "green"),
      menuItem("Hospitalization and death", 
               tabName = "statewide-hd", 
               icon = icon("hospital-symbol"),
               badgeLabel = "beta",
               badgeColor = "green"),
      menuItem("Tests", 
               tabName = "tests", 
               icon = icon("dna"),
               badgeLabel = "beta",
               badgeColor = "green"),
      menuItem("About", 
               tabName = "about", 
               icon = icon("info"),
               badgeLabel = "work in progress",
               badgeColor = "yellow")
    ),
    br(),
    box(
      title = "Settings",
      background = "green",
      width = 12,
      prettySwitch(inputId = "smoothToggle", 
                   label = "Moving Average",
                   value = TRUE)
    )
  ),
  dashboardBody(
    tabItems(
      source(file.path("ui", "statewide.R"),  local = TRUE)$value,
      source(file.path("ui", "county.R"),  local = TRUE)$value,
      source(file.path("ui", "statewide-death.R"),  local = TRUE)$value,
      source(file.path("ui", "statewide-hd.R"),  local = TRUE)$value,
      source(file.path("ui", "tests.R"),  local = TRUE)$value,
      source(file.path("ui", "about.R"),  local = TRUE)$value
    )
  )
)

# DEFINE SERVER
server <- function(input, output, session) {
  source(file.path("server", "statewide.R"),  local = TRUE)$value
  source(file.path("server", "county.R"),  local = TRUE)$value
  source(file.path("server", "statewide-death.R"),  local = TRUE)$value
  source(file.path("server", "statewide-hd.R"),  local = TRUE)$value
  source(file.path("server", "tests.R"),  local = TRUE)$value
  source(file.path("server", "about.R"),  local = TRUE)$value
} 

# RUN APPLICATION
shinyApp(ui = ui, server = server)
