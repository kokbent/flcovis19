library(tidyverse)
library(stringr)
library(lubridate)

folder <- "../flovid19-data/ctcase/"

#### Set 7-day timeframe
now_date <- Sys.Date()
week_ago_date <- now_date - ddays(7)

now_file <- paste0(folder, "ctcase_", str_remove_all(now_date, "-"), ".csv")
week_ago_file <- paste0(folder, "ctcase_", str_remove_all(week_ago_date, "-"), ".csv")

#### Read files, pick out test columns and calculate delta
dat_now <- read_csv(now_file)
dat_week_ago <- read_csv(week_ago_file)

dat_comb <- dat_now %>%
  select(County_1, T_positive, T_negative, T_total) %>%
  left_join(dat_week_ago %>% select(County_1, T_positive, T_negative, T_total),
            by = "County_1")

dat_comb <- dat_comb %>%
  mutate(week_tpos = T_positive.x - T_positive.y,
         week_tneg = T_negative.x - T_negative.y,
         week_ttot = T_total.x - T_total.y,
         pos_perc = week_tpos / week_ttot * 100)



#### Export
ct_pos_perc <- dat_comb %>%
  select(County = County_1, pos_perc, week_ttot)
ct_pos_perc$County[ct_pos_perc$County == "Dade"] <- "Miami-Dade"
ct_pos_perc$County <- toupper(ct_pos_perc$County)
write_csv(ct_pos_perc, "testApp/data/ct_pos_perc.csv")
