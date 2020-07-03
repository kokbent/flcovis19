tabItem(tabName = "about", 
        fluidRow(
          column(12, align = "center",
                 box(width = NULL, status = "warning",
                     h1("Hladish Lab's Data Visualization Hub for COVID-19"),
                     h3("Otherwise known as the FLovid-19 Data Visualizer 9000")
                 )#box
          )#column
        ),#fluidRow
        fluidRow(
          column(5, align = "center",
                 box(width = NULL, status = "primary",
                     h3("Hladish Lab Contributors"),
                     p(aboutText)
                 )#box
          ),#column
          column(7, align = "center",
                 box(width = NULL, status = "primary",
                     h3("About This Hub"),
                     p(aboutText)
                 )#box
          )#column
        )#fluidRow
)