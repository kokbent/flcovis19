tabPanel("Statewide Cases",
         fluidRow(
           column(12, align = "center",
                  h1("Number of Cases Statewide by Event Date"),
                  h4(plotStateDisclaimer),
                  h4(plotClickInstr),
                  plotOutput("stateCases", 
                             width = "90%",
                             height = "500px",
                             click = "stateClick", 
                             brush = brushOpts(id = "stateBrush", 
                                               fill = "#dfd", 
                                               direction = "x"))
           )#column
         ),#fluidRow
         fluidRow(
           column(1,
                  dropdownButton(inputId = "stateDropdown", 
                                 label = "Options", 
                                 up = TRUE, 
                                 icon = icon("gear"), 
                                 status = "primary",
                                 p("Toggle Smooth Line"),
                                 switchInput(inputId = "stateSmoothToggle", 
                                             value = TRUE)
                  ),
           ),
           column(6, 
                  align = "center",
                  box(title = "Selected Statewide Plot Data", 
                      width = NULL, 
                      status = "primary", 
                      solidHeader = TRUE,
                      DT::DTOutput("stateInfo")
                  )#box
           ),#column
           column(5, 
                  align = "center",
                  box(title = "Plot Information", 
                      width = NULL, 
                      status = "primary", 
                      solidHeader = TRUE,
                      p(strong("Event Date: "), eventDateDesc),
                      # p(strong("Number of Cases: "), "definition."),
                      p(strong("Reported case: "), reportedCaseDesc),
                      p(strong("Anticipated case: "), anticipatedCaseDesc)
                  )#box
           )#column
         )#fluidRow
)