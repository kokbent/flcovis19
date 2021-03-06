library(tidyverse)
library(lubridate)
library(googlesheets4)

if (packageVersion("googlesheets4") == "0.2.0") {
  gs4_auth("kokbent@gmail.com")
} else {
  sheets_auth("kokbent@gmail.com")
}

dat <- read_sheet("1EUr3mhs1PnN4HrF4HgYH1EalQwOgH1nwhHddpZ-fHJg")
dat <- dat %>%
  filter(!is.na(CRDeath))
dat$Date <- as.Date(dat$Date)

dat <- dat %>%
  arrange(Date) %>%
  mutate(TPC = Test/RCase_Incd,
         CPP = RCase_Incd/Pos,
         Death = c(diff(CRDeath), NA),
         Hosp = c(diff(CRHosp), NA))

dat$Case_ma7 <- stats::filter(dat$RCase_Incd, rep(1/7, 7), sides = 2)
dat$Death_ma7 <- stats::filter(dat$Death, rep(1/7, 7), sides = 2)
dat$Hosp_ma7 <- stats::filter(dat$Hosp, rep(1/7, 7), sides = 2)
dat$Weekend <- wday(dat$Date, label = T) %in% c("Sat", "Sun")
dat$Pos_perc <- dat$Pos / dat$Test

dat1 <- dat %>%
  select(Date, Test, Pos, Perc_Pos_NewCase, Pos_perc, Hosp, Death, Hosp_ma7, Death_ma7, Weekend)

write_csv(dat1, "testApp/data/statewide-thd.csv")
