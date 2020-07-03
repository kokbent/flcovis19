tabItem(tabName = "tests",
        fluidRow(
          column(6, align = "center",
                 h1("Statewide Daily Percentage of Positive Test Outcomes"),
                 h4(plotStatePosDisclaimer),
                 plotOutput("statePos",
                            width = "95%",
                            height = "500px")
          ),
          
          column(6, align = "center",
                 h1("Percentage of Positive Test Outcomes over past 7 days"),
                 h4(plotCountyPosDisclaimer),
                 leafletOutput("countyPos", 
                               width = "95%",
                               height = "500px")
          )
        )
)
