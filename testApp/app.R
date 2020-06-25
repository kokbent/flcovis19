library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)
library(lubridate)
library(DT)
#library(shinyjs)
library(shinydashboard)
library(shinyWidgets)
library(cowplot)
library(plotly)

# IMPORT DATA INITIALIZATION FUNCTIONS
source("dataImportClean.R")
source("loadingMessages.R")
source("text.R")

# DATA INITIALIZATION
getData() # Included dates stuff

# Codes and functions for plotting state and counties cases
source("plotsForStateCounties.R")

# GLOBAL OPTIONS
options(DT.options = list(searching = FALSE))

# DEFINE UI
ui <- tagList(
  useShinydashboard(),
      navbarPage("FLovid-19 Data Visualizer 9000",
                 source(file.path("ui", "statewide.R"),  local = TRUE)$value,
                 source(file.path("ui", "county.R"),  local = TRUE)$value,
                 source(file.path("ui", "about.R"),  local = TRUE)$value
                 # source(file.path("ui", "region.R"),  local = TRUE)$value
      )#navbarPage
)#tagList

# DEFINE SERVER
server <- function(input, output, session) {
  source(file.path("server", "statewide.R"),  local = TRUE)$value
  source(file.path("server", "county.R"),  local = TRUE)$value
  # source(file.path("server", "region.R"),  local = TRUE)$value
} 

# RUN APPLICATION
shinyApp(ui = ui, server = server)
