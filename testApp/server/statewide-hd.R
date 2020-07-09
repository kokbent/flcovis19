output$stateHosp <- renderPlotly({
  p <- plot_ly(THD_dat, 
               x = ~Date, 
               y = ~Hosp,
               type = "bar",
               color = ~Weekend,
               colors = c("#0072B2", "#56B4E9"),
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
              hoverinfo = "none")
  
  p %>%
    layout(legend = list(orientation = "h",
                         x = 0.5, y = 1,
                         xanchor = "center"),
           
           xaxis = list(
             title = "",
             rangeselector = list(
               buttons = list(
                 list(
                   count = 1,
                   label = "1 mo",
                   step = "month",
                   stepmode = "backward"),
                 list(
                   count = 2,
                   label = "2 mo",
                   step = "month",
                   stepmode = "backward"),
                 list(step = "all"))),
             
             rangeslider = list(type = "date")),
           
           yaxis = list(title = "New Hospitalization"))
})

output$stateDeath <- renderPlotly({
  p <- plot_ly(THD_dat, 
               x = ~Date, 
               y = ~Death,
               type = "bar",
               color = ~Weekend,
               colors = c("#0072B2", "#56B4E9"),
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
              hoverinfo = "none")
  
  p %>%
    layout(legend = list(orientation = "h",
                         x = 0.5, y = 1,
                         xanchor = "center"),
           
           xaxis = list(
             title = "",
             rangeselector = list(
               buttons = list(
                 list(
                   count = 1,
                   label = "1 mo",
                   step = "month",
                   stepmode = "backward"),
                 list(
                   count = 2,
                   label = "2 mo",
                   step = "month",
                   stepmode = "backward"),
                 list(step = "all"))),
             
             rangeslider = list(type = "date")),
           
           yaxis = list(title = "New Death")) %>%
    plotly_conf()
})