output$stateDeaths <- renderPlot({
  now_date <- max(death_preds$ChartDate)
  death_preds_long <- death_preds %>%
    select(EventDate, n, pred = mean_pred) %>%
    pivot_longer(-EventDate,
                 names_to = "type",
                 values_to = "n") %>%
    filter(!is.na(n))
  
  death_preds_long$type <- factor(death_preds_long$type, levels = c("pred", "n"))
  
  ggplot() +
    geom_col(aes(x=EventDate, y=n, fill=type), data=death_preds_long) +
    # geom_point(aes(x=EventDate, y=n), data=truth) +
    geom_errorbar(aes(x=EventDate, ymin=n+loCI, ymax=n+upCI), data=death_preds, width=0.25) +
    scale_fill_manual(name="", values=c("#d55e00", "#0072b2"),
                      labels=c("Anticipated", "Reported")) +
    scale_x_date(expand=c(0,0), date_breaks = "2 week", date_labels = "%b %d",
                 limits = c(ymd("2020-05-13"), now_date)) +
    scale_y_continuous(expand = c(0, 0), 
                       limits = c(0, max(death_preds$n + death_preds$upCI, na.rm = T) * 1.1)) +
    theme_bw() +
    theme(legend.position = "top", 
          plot.margin = margin(10, 30, 10, 10), 
          legend.text = element_text(size = 12),
          axis.text = element_text(size = 10), 
          axis.title = element_text(size = 15), 
          plot.caption = element_text(size = 12)) +
    labs(x = "", y = "",
         caption = paste0("Data updated as of ", now_date))
})