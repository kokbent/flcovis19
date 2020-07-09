tabItem(tabName = "county",
        fluidRow(
          column(12, align = "center",
                 h1("Reported cases for selected county"),
                 h4(plotCountyDisclaimer),
                 h4(plotClickInstr),
                 h3("Select county"),
                 selectInput("county", label = "", choices = names(split_counties)),
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
          column(8, align = "center",
                 box(title = "Selected data (use mouse to select from plot above)", width = NULL, status = "primary", solidHeader = TRUE,
                     DT::DTOutput("countyInfo")
                 )#box
          ),#column
          column(4, align = "center",
                 box(title = "Plot details", width = NULL, status = "primary", solidHeader = TRUE,
                     p(strong("Event date: "), "definition."),
                     p(strong("Reported cases: "), "definition."),
                     # p(strong("Trailing average: "), "definition."),
                     p(strong("Centered average: "), "definition.")
                 )#box
          )#column
        )#fluidRow
)