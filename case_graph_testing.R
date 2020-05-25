library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(lubridate)
library(stringr)

#GLOBAL DATE SETTINGS
effective_date <- Sys.Date() - ddays(14) # 2 weeks of "unreliable time"
today_date <- as.character(Sys.Date()) %>% str_remove_all("-")
display_date <- Sys.Date() %>% format(format = "%B %d, %Y") %>% as.character()

#CREATE NEW DIRECTORY FOR TODAY'S PLOTS
dir.create(paste0("plots/", today_date))

#IMPORT DATA FROM FDOH
in_file <- paste0("data/linelist_", today_date, ".csv")
download.file("https://opendata.arcgis.com/datasets/37abda537d17458bae6677b8ab75fcb9_0.csv",
              in_file)

dat <- read.csv(in_file)

#DATA CLEANUP FOR DATES
dat$EventDate <- ymd_hms(dat$EventDate) %>% as.Date()
dat$ChartDate <- ymd_hms(dat$ChartDate) %>% as.Date()

#STATEWIDE DATA AND VISUALIZATION

#GROUP AND COUNT CASES BY DATE
case_ev <- dat %>% 
  group_by(EventDate) %>% 
  count %>% 
  ungroup

#FILL IN MISSING DATES AND LABEL DAYS AND WEEKENDS
case_ev <- complete(case_ev, EventDate = seq.Date(min(case_ev$EventDate), max(case_ev$EventDate), by = 1),
                    fill = list(0)) %>%
  mutate(day = wday(EventDate, label = T),
         weekend = day %in% c("Sat", "Sun"),
         n = ifelse(is.na(n), 0, n))

#CALCULATE SEVEN-DAY TRAILING AVERAGE 
case_ev$ma7 <- stats::filter(case_ev$n, rep(1/7, 7), sides = 1)

#PLOT STATEWIDE DATA
case_ev %>%
  filter(EventDate >= ymd("2020-03-01")) %>%
  ggplot() +
  geom_col(aes(x = EventDate, y = n, fill = as.factor(weekend)), colour = "black", alpha = 0.75) +
  geom_line(aes(x = EventDate, y = ma7), lwd = 1.2) +
  annotate("rect", xmin = ymd(effective_date), xmax = max(case_ev$EventDate) + ddays(1), 
                ymin = -Inf, ymax = Inf, alpha = 0.5) +
  scale_fill_manual(name = "", labels = c("Weekday", "Weekend"),
                    values = c("#D55E00", "#0072B2")) +
  scale_x_date(expand=c(0,0), date_breaks = "1 week", date_labels = "%b %d",
               limits = c(ymd("2020-02-29"), max(case_ev$EventDate)+ddays(1))) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, max(case_ev$n) + 100),
                     breaks = 0:4 * 250) +
  theme_bw() +
  theme(legend.position = "top", plot.margin = margin(10, 30, 10, 10)) +
  labs(x = "Date", y = "Number of cases", 
       title = "Number of Cases Statewide by Event Date",
       subtitle = "The shaded region represents data from the last 14 days and is likely to be revised",
       caption = paste0("Data updated as of ", display_date))

#SAVE PLOT
ggsave(paste0("plots/", today_date, "/StatewideCases_", today_date, ".pdf"))
ggsave(paste0("plots/", today_date, "/StatewideCases_", today_date, ".png"))

#COUNTY-LEVEL DATA AND VISUALIZATION

#GROUP AND COUNT CASES BY COUNTY AND DATE
county_ct <- dat %>% 
  group_by(EventDate, County) %>% 
  count %>% 
  ungroup

#SPLIT DATA BY COUNTY
split_counties <- split(county_ct, county_ct$County)

#FILL IN MISSING DATES, LABEL DAYS, AND CALCULATE SEVEN-DAY TRAILING AVERAGE BY COUNTY
for (i in seq(split_counties)) {
  split_counties[[i]] <- complete(split_counties[[i]], EventDate = seq.Date(min(county_ct$EventDate), max(county_ct$EventDate), by = 1), 
                                  fill = list(0)) %>% 
    mutate(day = wday(EventDate, label = T), 
           weekend = day %in% c("Sat", "Sun"),
           n = ifelse(is.na(n), 0, n))
  
  split_counties[[i]]$ma7 <- stats::filter(split_counties[[i]]$n, rep(1/7, 7), sides = 1)
  
}

# USE THIS SECTION TO DISPLY EACH PLOT IN RSTUDIO
# lapply(seq(split_counties), function(i)
#     ggplot(split_counties[[i]]) +
#     geom_col(aes(x = EventDate, y = n, fill = as.factor(weekend)), colour = "black", alpha = 0.75) +
#     geom_line(aes(x = EventDate, y = ma7), lwd = 1.2) +
#      annotate("rect", xmin = ymd(effective_date), xmax = max(split_counties[[i]]$EventDate) + ddays(1),
#              ymin = -Inf, ymax = Inf, alpha = 0.5) +
#     scale_fill_manual(name = "", labels = c("Weekday", "Weekend"),
#                       values = c("#D55E00", "#0072B2")) +
#     scale_x_date(expand=c(0,0), date_breaks = "1 week", date_labels = "%b %d",
#                  limits = c(ymd("2020-02-29"), max(split_counties[[i]]$EventDate)+ddays(1))) +
#     scale_y_continuous(expand = c(0, 0), limits = c(0, max(split_counties[[i]]$n) + (0.1*max(split_counties[[i]]$n)))) +
#     theme_bw() +
#     theme(legend.position = "top", plot.margin = margin(10, 30, 10, 10)) +
#     labs(x = "Date", y = "Number of cases",
#          title = paste0("Number of Cases in ", names(split_counties[i]), " County by Event Date"),
#          subtitle = "The shaded region represents data from the last 14 days and is likely to be revised",
#          caption = paste0("Data updated as of ", display_date))
# )

#PLOT AND SAVE COUNTY-LEVEL DATA
for (i in seq(split_counties)) {
  ggplot(split_counties[[i]]) +
    geom_col(aes(x = EventDate, y = n, fill = as.factor(weekend)), colour = "black", alpha = 0.75) +
    geom_line(aes(x = EventDate, y = ma7), lwd = 1.2) +
    annotate("rect", xmin = ymd(effective_date), xmax = max(split_counties[[i]]$EventDate) + ddays(1), 
             ymin = -Inf, ymax = Inf, alpha = 0.5) +
    scale_fill_manual(name = "", labels = c("Weekday", "Weekend"),
                      values = c("#D55E00", "#0072B2")) +
    scale_x_date(expand=c(0,0), date_breaks = "1 week", date_labels = "%b %d",
                 limits = c(ymd("2020-02-29"), max(split_counties[[i]]$EventDate)+ddays(1))) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, max(split_counties[[i]]$n) + (0.1*max(split_counties[[i]]$n)))) +
    theme_bw() +
    theme(legend.position = "top", plot.margin = margin(10, 30, 10, 10)) +
    labs(x = "Date", y = "Number of cases", 
         title = paste0("Number of Cases in ", names(split_counties[i]), " County by Event Date"),
         subtitle = "The shaded region represents data from the last 14 days and is likely to be revised",
         caption = paste0("Data updated as of ", display_date))
  
  ggsave(paste0("plots/", today_date, "/", names(split_counties[i]), "Countycases_", today_date, ".pdf"))
  ggsave(paste0("plots/", today_date, "/", names(split_counties[i]), "Countycases_", today_date, ".png"))
}

