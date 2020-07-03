output$stateHosp <- renderPlotly(
  plot_ly(THD_dat, 
          x = ~Date, 
          y = ~Hosp,
          type = "bar",
          color = ~Weekend,
          colors = c("#D55E00", "#0072B2"),
          alpha = 0.75,
          hoverinfo = 'text',
          text = ~paste('<b>', Date, '</b></br>', 
                        '</br> New Hospitalization: ', Hosp,
                        '</br> 7-day Moving Average: ', round(Hosp_ma7))) %>%
    add_lines(x = ~Date,
              y = ~Hosp_ma7,
              line = list(color = "black"),
              inherit = F,
              name = '7-day moving average (centered)',
              hoverinfo = "none") %>%
    layout(legend = list(orientation = "h",
                         x = 0.5,
                         xanchor = "center"),
           xaxis = list(title = ""),
           yaxis = list(title = "New Hospitalization"))
)

output$stateDeath <- renderPlotly(
  plot_ly(THD_dat, 
          x = ~Date, 
          y = ~Death,
          type = "bar",
          color = ~Weekend,
          colors = c("#D55E00", "#0072B2"),
          alpha = 0.75,
          hoverinfo = 'text',
          text = ~paste('<b>', Date, '</b></br>', 
                        '</br> New Death: ', Death,
                        '</br> 7-day Moving Average: ', round(Death_ma7))) %>%
    add_lines(x = ~Date,
              y = ~Death_ma7,
              line = list(color = "black"),
              inherit = F,
              name = '7-day moving average (centered)',
              hoverinfo = "none") %>%
    layout(legend = list(orientation = "h",
                         x = 0.5,
                         xanchor = "center"),
           xaxis = list(title = ""),
           yaxis = list(title = "New Death"))
)