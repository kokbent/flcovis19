tabItem(tabName = "county",
        fluidRow(
          column(12, align = "center",
                 h1("Reported cases for selected county"),
                 h4(plotCountyDisclaimer),
                 h4(plotClickInstr),
                 h3("Select county"),
                 selectInput("county", label = "", choices = unique(countyData$County)),
                 plotlyOutput("countyCases", 
                              width = "90%",
                              height = "600px")
                 # plotOutput("countyCases", 
                 #            width = "90%",
                 #            height = "500px",
                 #            click = "countyClick", 
                 #            brush = brushOpts(id = "countyBrush", fill = "#dfd", direction = "x"))
          )#column
        ),#fluidRow
        hr(),
        fluidRow(
                column(2),
                column(8,
                       box(title = "Plot details", width = NULL, status = "primary", solidHeader = TRUE,
                           p(strong("Event date: "), eventDateDesc)
                       )
                ),
                column(2)
        )#fluidRow
)