library(tidyverse)
library(surveillance)

#### Data
download.file("https://opendata.arcgis.com/datasets/37abda537d17458bae6677b8ab75fcb9_0.csv", 
              "data/linelist.csv")
line_list <- read_csv("data/linelist.csv")

line_list$EventDate <- ymd_hms(line_list$EventDate) %>% as.Date
line_list$ChartDate <- ymd_hms(line_list$ChartDate) %>% as.Date
line_list$ret_day <- as.numeric(line_list$ChartDate - line_list$EventDate)

#### Data carpentry
## Specify start date and nowcast date.
## Choose a starting date in which the trend (of case reporting) has stabilized,
## typically this would be at least one or two week before the nowcasting window.
start_date <- ymd("2020-04-20")
nowcast_date <- ymd("2020-05-17")

## For this exercise purpose, we go back to how linelist would look like on the nowcast date
line_list_now <- line_list %>%
  filter(ChartDate <= nowcast_date)

## Filter out cases limiting it to what you would have seen on the nowcasting date,
## which is usually a trapezium. Consider only 14-day window.
## Also remove negative "return day" which happen :/
date_train <- line_list_now %>%
  filter(EventDate >= start_date, EventDate <= nowcast_date) %>%
  filter(ret_day >= 0, ret_day <= 14) %>%
  filter(ret_day <= as.numeric(nowcast_date - EventDate)) %>%
  dplyr::select(EventDate, ChartDate, ret_day)

#### Nowcasting
nc.control <- list(N.tInf.prior=structure("poisgamma",
                                          mean.lambda=50,var.lambda=3000),
                   N.tInf.max=1300,
                   nSamples=1e3)

when <- seq(nowcast_date-ddays(13), nowcast_date, by=1) # Window to nowcast

nc <- surveillance::nowcast(now=nowcast_date, when=when,
                            dEventCol="EventDate", dReportCol="ChartDate",
                            data=as.data.frame(date_train), D=14, method="bayes.trunc",
                            control=nc.control, m=14)

#### Extract predictions
pred_df <- data.frame(EventDate = seq(start_date, nowcast_date, by=1),
                      mean = as.vector(nc@upperbound),
                      upCI = as.vector(nc@pi[,,2]),
                      loCI = as.vector(nc@pi[,,1]))

#### Carpentry for visualization

## date_train is restricted to 14-day window for model training
## But in actual visualization, we show actual number of cases reported up to the
## nowcast date, i.e. Case counts for EventDate outside of 14-day window should 
## be larger than those derived from date_train.

case_actual <- line_list_now %>%
  filter(EventDate >= start_date, EventDate <= nowcast_date) %>%
  filter(ret_day >= 0) %>%
  group_by(EventDate) %>%
  summarise(n=n())

case_train <- date_train %>%
  group_by(EventDate) %>%
  summarise(n=n())

## For demonstration purpose of difference between 14-day count vs "truth" count:
plot(case_actual$n, type="l", ylim=c(0, 900))
lines(case_train$n, lty=2) # case_train < truth day 14 onwards, but = truth otherwise

## Adding predictions and pivot table in case_actual
case_actual$pred <- pred_df$mean - case_actual$n # pred is how many more cases anticipated

case_actual_long <- case_actual %>%
  pivot_longer(-EventDate, names_to = "type", values_to = "n", values_drop_na = T)
case_actual_long$type <- factor(case_actual_long$type, levels=c("pred", "n"))

## One more thing, case_actual is based on the reality up to the nowcasting date.
## But for model validation, we have the "truth" which is the observed 14-day case count
## for each of the 14 days casting window (e.g. total case count 14 days after day 0,
## 13 days after day 1, ...) Those "truth" is used for model validation.
truth <- line_list %>%
  filter(EventDate %in% when) %>% # when is the 14-day nowcasting window
  filter(ret_day >= 0, ret_day <= 14) %>%
  group_by(EventDate) %>%
  summarise(n=n())

#### Graphing
ggplot() +
  geom_col(aes(x=EventDate, y=n, fill=type), data=case_actual_long) +
  geom_point(aes(x=EventDate, y=n), data=truth) +
  geom_errorbar(aes(x=EventDate, ymin=loCI, ymax=upCI), data=pred_df, width=0.25) +
  scale_fill_manual(name="", values=c("#E69F00", "#56B4E9"),
                    labels=c("Anticipated", "Reported")) +
  labs(title = "Bayesian Poisson model",
       subtitle = "from surveillance::nowcast")
