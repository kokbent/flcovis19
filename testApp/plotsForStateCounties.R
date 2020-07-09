## STATEPLOT DATA CARPENTRY
pred_nowcast <- pred_df %>%
  select(EventDate, n, pred, loCI, upCI) %>%
  filter(!is.na(n))
nowcast_date <- max(pred_df$EventDate)

# MANIPULATE ACTUAL DATA
case_ev_plot <- case_ev %>%
  filter(EventDate >= ymd("2020-03-01"), EventDate <= nowcast_date) %>%
  select(-day, -ma7, -ca7)
case_ev_plot <- left_join(case_ev_plot, pred_nowcast %>% select(EventDate, pred))

# PRED MANIPULATE FOR PLOTLY
pred_df$upCI2 <- with(pred_df, upCI - n - pred)
pred_df$loCI2 <- with(pred_df, n + pred - loCI)
pred_df1 <- pred_df %>%
  filter(!is.na(pred))

## STATEPLOT
# LAYERS
statePlot <- plot_ly() %>%
  add_bars(x = ~EventDate,
           y = ~n,
           color = ~as.factor(weekend),
           colors = c("#0072B2", "#56b4e9"),
           name = ~ifelse(weekend, "Weekend Reported", "Weekday, "),
           data = case_ev_plot,
           text = ~paste('<b>', EventDate, '</b></br>', 
                         '</br>Reported cases (to date): ', n),
           hoverinfo = "text") %>%
  add_bars(x = ~EventDate,
           y = ~(pred + n),
           name = 'Anticipated total',
           error_y = list(type = "data",
                          symmetric = F,
                          array = ~upCI2, 
                          arrayminus = ~loCI2,
                          color = "d55e00",
                          thickness = 1.5,
                          width = 1.5),
           marker = list(color = "#88888811",
                         line = list(color = "#d55e00",
                                     width = 1.5)),
           data = pred_df1,
           text = ~paste('<b>', EventDate, '</b></br>', 
                         '</br>Anticipated Total:', round(pred + n),
                         '(', round(loCI), '-', round(upCI), ')'),
           hoverinfo = "text") %>%
  add_lines(x = ~EventDate,
            y = ~ma7_mean,
            data = pred_df,
            line = list(color = "black"),
            name = '7-day moving average (centered)',
            hoverinfo = "none",
            visible = "legendonly") %>%
  add_ribbons(x = ~EventDate,
              ymax = ~ma7_upCI,
              ymin = ~ma7_loCI,
              data = pred_df,
              line = list(color = 'rgba(100, 100, 100, 0)'),
              fillcolor = 'rgba(100, 100, 100, 0.5)',
              name = '95% prediction interval (moving average)',
              hoverinfo = "none",
              visible = "legendonly")

# STATE PLOT LAYOUT AND CONFIG
statePlot <- statePlot %>%
  layout(
    legend = list(orientation = "h",
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
    
    yaxis = list(title = "Reported Case",
                 range = c(0, max(pred_df1$upCI, na.rm = T)*1.1)),
    
    barmode = "overlay",
    
    hovermode = "compare"
  ) %>%
  plotly_conf()


# COUNTY PLOT
# countyPlotly <- plot_ly() %>%
#   add_bars(x = ~EventDate,
#            y = ~n,
#            color = ~as.factor(weekend),
#            colors = c("#0072B2", "#56b4e9"),
#            name = ~ifelse(weekend, "Weekend Reported", "Weekday, "),
#            text = ~paste('<b>', EventDate, '</b></br>', 
#                          '</br>Reported cases (to date): ', n),
#            hoverinfo = "text") %>%
#   add_lines(x = ~EventDate,
#             y = ~ca7,
#             line = list(color = "black"),
#             name = '7-day moving average (centered)',
#             hoverinfo = "none",
#             visible = "legendonly") %>%
#   layout(
#     legend = list(orientation = "h",
#                   x = 0.5, y = 1,
#                   xanchor = "center"),
#     
#     xaxis = list(
#       title = "",
#       rangeselector = list(
#         buttons = list(
#           list(
#             count = 1,
#             label = "1 mo",
#             step = "month",
#             stepmode = "backward"),
#           list(
#             count = 2,
#             label = "2 mo",
#             step = "month",
#             stepmode = "backward"),
#           list(step = "all"))),
#       
#       rangeslider = list(type = "date")),
#     
#     yaxis = list(title = "Reported Case"),
#     
#     barmode = "overlay",
#     
#     hovermode = "compare"
#   ) %>%
#   plotly_conf()
# 
# 
# 
# ## COUNTY PLOT
# countyPlot <- function(county){
#   ggplot(split_counties[[county]]) +
#     geom_col(aes(x = EventDate, y = n, fill = as.factor(weekend)), alpha = 0.75) +
#     #geom_line(aes(x = EventDate, y = ma7), lwd = 1.2) +
#     annotate("rect", xmin = ymd(effective_date), xmax = max(split_counties[[county]]$EventDate) + ddays(1), 
#              ymin = -Inf, ymax = Inf, alpha = 0.5) +
#     scale_fill_manual(name = "", labels = c("weekday", "weekend"),
#                       values = c("#D55E00", "#0072B2")) +
#     scale_x_date(expand=c(0,0), date_breaks = "1 week", date_labels = "%b %d",
#                  limits = c(ymd("2020-02-29"), max(split_counties[[county]]$EventDate)+ddays(1))) +
#     scale_y_continuous(expand = c(0, 0), limits = c(0, max(split_counties[[county]]$n) + (0.1*max(split_counties[[county]]$n)))) +
#     theme_bw() +
#     theme(legend.position = "top", plot.margin = margin(10, 30, 10, 10), legend.text = element_text(size = 15, face = "bold"), 
#           axis.text = element_text(size = 10), axis.title = element_text(size = 15), plot.caption = element_text(size = 12)) +
#     labs(x = "Event date", y = "Reported cases",
#          caption = paste0("Data updated as of ", display_date))
# }