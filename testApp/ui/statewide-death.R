tabItem(tabName = "statewide-death",
        fluidRow(
          align = "center",
          h1("Reported COVID-19 Death (by day of death) for Florida"),
          plotOutput("stateDeaths", 
                     width = "90%",
                     height = "600px")
        ),
        hr(),
        fluidRow(
          column(2),
          column(8,
                 box(title = "Plot details", 
                     width = NULL, 
                     status = "primary", 
                     solidHeader = TRUE,
                     p("Florida DOH publishes and updates the reported COVID-19 Death by day of death in its dashboard.
                       However, the graph they created is limited to 30-day window and there are significant reporting 
                       delay -- substantial revision of the numbers can still occur beyond 30 days. Here we use a 
                       Bayesian nowcasting model to anticipate the revised number of death at ",
                       em("day 21 after the day of death. "),
                       "The numbers stabilise but can continue to rise even after day 21.")
                     # p(strong("Number of Cases: "), "definition."),
                 )#box
          ),
          column(2)
        )
)

