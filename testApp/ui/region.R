## Regional plot
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