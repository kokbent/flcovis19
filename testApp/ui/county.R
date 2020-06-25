tabPanel("County-Level Cases",
         fluidRow(
           column(12, align = "center",
                  h1("Number of Cases in Selected County by Event Date"),
                  h4(plotCountyDisclaimer),
                  h4(plotClickInstr),
                  h3("Select County"),
                  selectInput("county", label = "", choices = names(split_counties)),
                  plotOutput("countyCases", click = "countyClick", brush = brushOpts(id = "countyBrush", fill = "#dfd", direction = "x"))
           )#column
         ),#fluidRow
         fluidRow(
           column(1,
                  dropdownButton(inputId = "countyDropdown", label = "Options", up = TRUE, icon = icon("gear"), status = "primary",
                                 p("Toggle Smooth Line"),
                                 switchInput(inputId = "countySmoothToggle", value = TRUE)
                  ),
           ),
           column(8, align = "center",
                  box(title = "Selcted County-Level Plot Data", width = NULL, status = "primary", solidHeader = TRUE,
                      DT::DTOutput("countyInfo")
                  )#box
           ),#column
           column(3, align = "center",
                  box(title = "Plot Information", width = NULL, status = "primary", solidHeader = TRUE,
                      p(strong("Event Date: "), "definition."),
                      p(strong("Number of Cases: "), "definition."),
                      p(strong("Trailing Average: "), "definition."),
                      p(strong("Centered Average: "), "definition.")
                  )#box
           )#column
         )#fluidRow
)