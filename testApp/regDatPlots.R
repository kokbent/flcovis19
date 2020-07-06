
#DATA CLEANUP, COUNT, DATE FIXING BY REGION
reg_Dat <- list() %>% 
  append(list(left_join(region_1, dat, by = c("County" = "County")))) %>%
  append(list(left_join(region_2, dat, by = c("County" = "County")))) %>% 
  append(list(left_join(region_3, dat, by = c("County" = "County")))) %>% 
  append(list(left_join(region_4, dat, by = c("County" = "County")))) %>% 
  append(list(left_join(region_5, dat, by = c("County" = "County")))) %>% 
  append(list(left_join(region_6, dat, by = c("County" = "County"))))


reg_ct <- function(region){
  region %>% 
    group_by(EventDate) %>%
    count %>%
    ungroup
}

reg_mutate <- function(region){
  complete(region, EventDate = seq.Date(as.Date("2020/1/1"), Sys.Date(), by = 1),
           fill = list(0)) %>%
    mutate(day = wday(EventDate, label = T),
           weekend = day %in% c("Sat", "Sun"),
           n = ifelse(is.na(n), 0, n))
}

reg_list <- list()
reg_listFilter <- list()

for(r in seq(length(reg_Dat))){
  reg_list <- append(reg_list, list(reg_Dat[[r]] %>% 
                                           reg_ct %>% 
                                           reg_mutate))
}

for(j in seq(length(reg_list))){
  reg_listFilter <- append(reg_listFilter, list(dplyr::filter(reg_list[[j]], (is.na(reg_list[[j]][["EventDate"]]) == FALSE))))
}

for(reg in seq(length(reg_listFilter))){
  reg_listFilter[[reg]][["ma7"]] <- stats::filter(reg_listFilter[[reg]][["n"]], rep(1/7, 7), sides = 1)
  reg_listFilter[[reg]][["ca7"]] <- stats::filter(reg_listFilter[[reg]][["n"]], rep(1/7, 7), sides = 2)
}

reg_Plot <- function(regionNum){
  ggplot(reg_listFilter[[regionNum]]) +
    geom_col(aes(x = EventDate, y = n, fill = as.factor(weekend)), colour = "black", alpha = 0.75) +
    geom_line(aes(x = EventDate, y = ca7), lwd = 1.2) +
    annotate("rect", xmin = ymd(effective_date), xmax = max(reg_listFilter[[regionNum]]$EventDate) + ddays(1), 
             ymin = -Inf, ymax = Inf, alpha = 0.5) +
    scale_fill_manual(name = "", labels = c("Weekday", "Weekend"),
                      values = c("#D55E00", "#0072B2")) +
    scale_x_date(expand=c(0,0), date_breaks = "1 week", date_labels = "%b %d",
                 limits = c(ymd("2020-02-29"), max(reg_listFilter[[regionNum]]$EventDate)+ddays(1))) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, max(reg_listFilter[[regionNum]]$n) + (0.1*max(reg_listFilter[[regionNum]]$n)))) +
    theme_bw() +
    theme(legend.position = "top", plot.margin = margin(10, 30, 10, 10)) +
    labs(x = "Date", y = "Reported cases", 
         title = paste0("Aggregate Number of Cases in Region ", regionNum, " by Event Date"),
         subtitle = "The shaded region represents data from the last 14 days and is likely to be revised",
         caption = paste0("Data updated as of ", display_date))
}
