pred_df <- read_csv("data/statewide-nowcast-preds.csv")

pred_nowcast <- pred_df %>%
  select(EventDate, n, pred, loCI, upCI) %>%
  filter(!is.na(n))
nowcast_date <- max(pred_df$EventDate)

case_ev_plot <- case_ev %>%
  filter(EventDate >= ymd("2020-03-01"), EventDate <= nowcast_date) %>%
  select(-day, -ma7, -ca7)
case_ev_plot <- left_join(case_ev_plot, pred_nowcast %>% select(EventDate, pred))

#PIVOT TO LONG FORM FOR PLOTTING
case_ev_plot <- case_ev_plot %>%
  pivot_longer(-c("EventDate", "weekend"), 
               names_to = "type", 
               values_to = "n", 
               values_drop_na = T)
case_ev_plot$type <- factor(case_ev_plot$type, levels=c("pred", "n"))
case_ev_plot$wknd_type <- paste(as.numeric(case_ev_plot$weekend), 
                                case_ev_plot$type)
case_ev_plot$wknd_type <- factor(case_ev_plot$wknd_type,
                                     levels = c("0 pred", "1 pred", "0 n", "1 n"))

ylim_max <- max(case_ev_plot$n, pred_df$upCI, na.rm = T)

#CREATE NEW DATA FRAME FOR INTERACTIVE PLOT DATA
case_df <- case_ev_plot %>% filter(type == "n") %>%
  select(EventDate, n)

case_df <- left_join(case_df, case_ev_plot %>% filter(type == "pred") %>% select(EventDate, n), by = "EventDate") %>% 
  rename(n = n.x, pred = n.y)

case_df <- left_join(case_df, pred_df %>% select(EventDate, ma7_mean), by = "EventDate")

case_df$pred <- case_df$pred %>% round(digits = 1)
case_df$ma7_mean <- case_df$ma7_mean %>% round(digits = 1)

## STATE PLOT
statePlot <- ggplot() +
  geom_col(aes(x = EventDate, y = n, fill = wknd_type), 
           data = case_ev_plot,
           alpha = 0.75) +
  geom_errorbar(aes(x = EventDate, ymin = loCI, ymax = upCI), data = pred_df, width = 0.25) +
  scale_fill_manual(name="", values=c("#FF7000", "#00A3FF", 
                                      "#813800", "#00588B"),
                    labels=c("", "Anticipated Cases (Weekday/Weekend)", 
                             "", "Reported Cases (Weekday/Weekend)")) +
  scale_x_date(expand=c(0,0), date_breaks = "2 week", date_labels = "%b %d",
               limits = c(ymd("2020-02-29"), nowcast_date+ddays(1))) +
  scale_y_continuous(expand = c(0, 0), 
                     limits = c(0, ylim_max + 100)) +
  theme_bw() +
  theme(legend.position = "top", 
        plot.margin = margin(10, 30, 10, 10), 
        legend.text = element_text(size = 14),
        axis.text = element_text(size = 13), 
        axis.title = element_text(size = 15), 
        plot.caption = element_text(size = 13)) +
  labs(x = "Date", y = "Number of cases",
       caption = paste0("Data updated as of ", display_date))

## COUNTY PLOT
countyPlot <- function(county){
  ggplot(split_counties[[county]]) +
    geom_col(aes(x = EventDate, y = n, fill = as.factor(weekend)), alpha = 0.75) +
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