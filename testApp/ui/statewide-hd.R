tabItem(tabName = "statewide-hd",
        fluidRow(
          column(12, align = "center",
                 h1("Statewide Daily Number of New Reported Hospitalization"),
                 h4(plotStateHospDisclaimer),
                 # h4(plotClickInstr),
                 plotlyOutput("stateHosp", 
                            width = "90%",
                            height = "500px")
          )#column
        ),#fluidRow
        hr(),
        fluidRow(
          column(12, align = "center",
                 h1("Statewide Daily Number of New Reported Death"),
                 h4(plotStateDeathDisclaimer),
                 # h4(plotClickInstr),
                 plotlyOutput("stateDeath", 
                              width = "90%",
                              height = "500px")
          )#column
        )
)