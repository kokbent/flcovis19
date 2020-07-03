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

week_tpos <- dat_now$T_positive - dat_week_ago$T_positive
week_tneg <- dat_now$T_negative - dat_week_ago$T_negative
week_ttot <- dat_now$T_total - dat_week_ago$T_total
pos_perc <- week_tpos / week_ttot * 100


#### Export
ct_pos_perc <- data.frame(County = dat_now$County_1, pos_perc = pos_perc,
                          week_ttot = week_ttot, stringsAsFactors = F)
ct_pos_perc$County[ct_pos_perc$County == "Dade"] <- "Miami-Dade"
ct_pos_perc$County <- toupper(ct_pos_perc$County)
write_csv(ct_pos_perc, "testApp/data/ct_pos_perc.csv")
