tabItem(tabName = "statewide",
        fluidRow(
          column(12, align = "center",
                 h1("Reported cases for Florida"),
                 h4(plotStateDisclaimer),
                 h4(plotClickInstr),
                 plotlyOutput("stateCases", 
                              width = "90%",
                              height = "600px")
          )#column
        ),#fluidRow
        hr(),
        fluidRow(
                column(2),
                column(8,
                       box(title = "Plot details", 
                           width = NULL, 
                           status = "primary", 
                           solidHeader = TRUE,
                           p(strong("Event date: "), eventDateDesc),
                           # p(strong("Number of Cases: "), "definition."),
                           p(strong("Reported cases: "), reportedCaseDesc),
                           p(strong("Anticipated cases: "), anticipatedCaseDesc)
                       )#box
                ),
                column(2)
        )
)