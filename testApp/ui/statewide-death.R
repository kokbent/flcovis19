tabItem(tabName = "statewide-death",
        fluidRow(
          align = "center",
          h1("Reported COVID-19 Death (by day of death) for Florida"),
          plotOutput("stateDeaths", 
                     width = "90%",
                     height = "600px")
        )
)

