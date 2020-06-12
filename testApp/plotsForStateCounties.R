statePlot <- case_ev %>%
  filter(EventDate >= ymd("2020-03-01")) %>%
  ggplot() +
  geom_col(aes(x = EventDate, y = n, fill = as.factor(weekend)), colour = "black", alpha = 0.75) +
  annotate("rect", xmin = ymd(effective_date), xmax = max(case_ev$EventDate) + ddays(1),
           ymin = -Inf, ymax = Inf, alpha = 0.5) +
  scale_fill_manual(name = "", labels = c("Weekday", "Weekend"),
                    values = c("#D55E00", "#0072B2")) +
  scale_x_date(expand=c(0,0), date_breaks = "1 week", date_labels = "%b %d",
               limits = c(ymd("2020-02-29"), max(case_ev$EventDate)+ddays(1))) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, max(case_ev$n) + 100),
                     breaks = 0:4 * 250) +
  theme_bw() +
  theme(legend.position = "top", plot.margin = margin(10, 30, 10, 10), legend.text = element_text(size = 15, face = "bold"),
        axis.text = element_text(size = 10), axis.title = element_text(size = 15), plot.caption = element_text(size = 12)) +
  labs(x = "Date", y = "Number of cases",
       caption = paste0("Data updated as of ", display_date))

countyPlot <- function(county){
  ggplot(split_counties[[county]]) +
    geom_col(aes(x = EventDate, y = n, fill = as.factor(weekend)), colour = "black", alpha = 0.75) +
    #geom_line(aes(x = EventDate, y = ma7), lwd = 1.2) +
    annotate("rect", xmin = ymd(effective_date), xmax = max(split_counties[[county]]$EventDate) + ddays(1), 
             ymin = -Inf, ymax = Inf, alpha = 0.5) +
    scale_fill_manual(name = "", labels = c("Weekday", "Weekend"),
                      values = c("#D55E00", "#0072B2")) +
    scale_x_date(expand=c(0,0), date_breaks = "1 week", date_labels = "%b %d",
                 limits = c(ymd("2020-02-29"), max(split_counties[[county]]$EventDate)+ddays(1))) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, max(split_counties[[county]]$n) + (0.1*max(split_counties[[county]]$n)))) +
    theme_bw() +
    theme(legend.position = "top", plot.margin = margin(10, 30, 10, 10), legend.text = element_text(size = 15, face = "bold"), 
          axis.text = element_text(size = 10), axis.title = element_text(size = 15), plot.caption = element_text(size = 12)) +
    labs(x = "Date", y = "Number of cases",
         caption = paste0("Data updated as of ", display_date))
}