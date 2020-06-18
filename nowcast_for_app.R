rm(list=ls())

library(tidyverse)
library(lubridate)
library(nimble)


#### Data
ll <- read.csv("data/linelist.csv")

ll$EventDate <- ymd_hms(ll$EventDate) %>% as.Date()
ll$ChartDate <- ymd_hms(ll$ChartDate) %>% as.Date()
ll$ret_day <- as.numeric(ll$ChartDate - ll$EventDate)


#### Data carpentry
## Specify start date and nowcast date.
## For our model purpose, start_date should preferably be 3 weeks before nowcast date
dat_date <- max(ll$EventDate)
nowcast_date <- dat_date - ddays(2)
start_date <- nowcast_date - ddays(30)

## Training dataset (Slice out the reporting trapezium)
case_rt_train <- ll %>%
  filter(EventDate >= start_date, EventDate <= nowcast_date) %>%
  filter(ret_day >= 0, ret_day <= 14) %>%
  # filter(ret_day <= as.numeric(nowcast_date - EventDate)) %>%
  group_by(EventDate, ret_day) %>%
  summarise(n=n())

# Complete the trapezium with zeros if needed
case_rt_train <- case_rt_train %>%
  ungroup() %>%
  complete(EventDate = seq(start_date, nowcast_date, by = 1),
           ret_day = 0:14,
           fill = list(n = 0)) %>%
  filter(ret_day <= as.numeric(nowcast_date - EventDate))

case_rt_train$EventDate2 <- as.numeric(case_rt_train$EventDate - start_date) + 1
case_rt_train$hol <- wday(case_rt_train$EventDate, label = T) %in% c("Sat", "Sun")
# case_rt_train$hol <- ifelse(case_rt_train$EventDate == ymd("2020-05-25"),
#                             TRUE,
#                             case_rt_train$hol)


#### Fit with NIMBLE ----
constants <- list(N = nrow(case_rt_train),
                  NED = max(case_rt_train$EventDate2),
                  NRD = max(case_rt_train$ret_day) + 1,
                  ed = case_rt_train$EventDate2,
                  rd = case_rt_train$ret_day + 1)

data <- list(y = case_rt_train$n,
             hol = as.numeric(case_rt_train$hol))
inits <- list(alpha = rep(0, constants$NED), beta = rep(0, constants$NRD), delta = 0,
              gamma = matrix(0, constants$NED, constants$NRD), r = 5)

source("nowcast_nb_JAGS.R")

system.time(model <- nimbleModel(code, constants = constants, data = data, inits = inits,
                                 calculate = T))
conf <- configureMCMC(model, 
                      monitors = c("mu", "alpha", "beta", "gamma", "r", "delta"))
mcmc <- buildMCMC(conf)
modelc <- compileNimble(model, showCompilerOutput = T)
mcmcc <- compileNimble(mcmc, showCompilerOutput = T)
# Uncomment for quick sanity check
# samp <- runMCMC(mcmcc, niter = 5000, nburnin = 2500, thin = 1, nchains = 1, 
#                 progressBar = T, samples = T, samplesAsCodaMCMC = T)
samp <- runMCMC(mcmcc, niter = 100000, nburnin = 50000, thin = 10, nchains = 3,
                setSeed = c(4326, 4327, 4328), progressBar = T,
                samples = T, samplesAsCodaMCMC = T)
summ <- MCMCvis::MCMCsummary(samp)


#### Extract Posterior Samples
pos <- do.call(rbind, samp)
pos_mu <- pos[,"mu"]
pos_alpha <- pos[,str_detect(colnames(pos), "alpha")]
pos_beta <- pos[,str_detect(colnames(pos), "beta")]
pos_gamma <- pos[,str_detect(colnames(pos), "gamma")]
pos_delta <- pos[,str_detect(colnames(pos), "delta")]
pos_r <- pos[,"r"]
n_pos <- length(pos_r)


#### Test set
case_rt_test <- data.frame(EventDate = seq(nowcast_date - ddays(13), nowcast_date, by = 1),
                           ret_day = 14:1)
case_rt_test$EventDate2 <- as.numeric(case_rt_test$EventDate - start_date) + 1
case_rt_test$hol <- wday(case_rt_test$EventDate, label = T) %in% c("Sat", "Sun")
# case_rt_test$hol <- ifelse(case_rt_test$EventDate == ymd("2020-05-25"),
#                            TRUE,
#                            case_rt_test$hol)


#### Extract predictions
pred_mat <- matrix(NA, n_pos, nrow(case_rt_test))
for (j in 1:nrow(case_rt_test)) {
  ind_ED <- case_rt_test$EventDate2[j]
  ind_RD <- case_rt_test$ret_day[j]
  hol <- case_rt_test$hol[j]
  
  gamma_colnames <- paste0("gamma[", ind_ED, ", ", (ind_RD+1):15, "]")
  cond <- colnames(pos_gamma) %in% gamma_colnames
  
  lc <- exp(pos_mu + pos_alpha[,ind_ED] + pos_beta[,(ind_RD+1):15] + 
              pos_gamma[,cond] + pos_delta * hol)
  
  preds <- rnbinom(n_pos*(15 - ind_RD), 
                   size = pos_r,
                   mu = lc) %>%
    matrix(n_pos)
  pred_mat[,j] <- rowSums(preds)
}
head(pred_mat)


#### Carpentry for visualization
case_actual <- ll %>%
  filter(EventDate >= ymd("2020-03-01"), EventDate <= nowcast_date) %>%
  filter(ret_day >= 0) %>%
  group_by(EventDate) %>%
  summarise(n=n())

case_train <- case_rt_train %>%
  group_by(EventDate) %>%
  summarise(n=sum(n))

## Adding predictions and pivot table in case_actual
case_actual$pred <- NA
ind <- nrow(case_actual) - nrow(case_rt_test) + 1
case_actual$pred[ind:nrow(case_actual)] <- colMeans(pred_mat)

case_actual_long <- case_actual %>%
  pivot_longer(-EventDate, names_to = "type", values_to = "n", values_drop_na = T)
case_actual_long$type <- factor(case_actual_long$type, levels=c("pred", "n"))

# Cross the factors to create visual
case_actual_long <- case_actual_long %>%
  mutate(day = wday(EventDate, label = T),
         weekend = as.numeric(day %in% c("Sat", "Sun")),
         wknd_type = paste(weekend, type))
case_actual_long$wknd_type <- factor(case_actual_long$wknd_type,
                                     levels = c("0 pred", "1 pred", "0 n", "1 n"))

## Prediction CI
pred_df <- data.frame(EventDate = case_rt_test$EventDate,
                      loCI = apply(pred_mat, 2, quantile, 0.025),
                      upCI = apply(pred_mat, 2, quantile, 0.975))
pred_df <- left_join(pred_df, case_actual)
pred_df$loCI <- pred_df$loCI + pred_df$n
pred_df$upCI <- pred_df$upCI + pred_df$n

## 7-day Moving Average
ma7_mat <- matrix(NA, n_pos, nrow(case_actual))
for (i in 1:n_pos) {
  ts_n <- case_actual$n + 
    c(rep(0, nrow(case_actual) - nrow(case_rt_test)), pred_mat[i,])
  ma7_mat[i,] <- stats::filter(ts_n, rep(1/7, 7), sides = 2)
}

ma7_df <- data.frame(EventDate = case_actual$EventDate,
                     mean = colMeans(ma7_mat),
                     loCI = apply(ma7_mat, 2, quantile, 0.025, na.rm = T),
                     upCI = apply(ma7_mat, 2, quantile, 0.975, na.rm = T))
cond <- ma7_df$loCI ==  ma7_df$upCI
ma7_df$loCI[cond] <- NA
ma7_df$upCI[cond] <- NA


#### Graphing
ggplot() +
  geom_col(aes(x=EventDate, y=n, fill=wknd_type), data=case_actual_long,
           alpha = 0.9) +
  geom_errorbar(aes(x=EventDate, ymin=loCI, ymax=upCI), data=pred_df, width=0.25) +
  geom_line(aes(x = EventDate, y = mean), data = ma7_df, lwd = 1.1) +
  geom_ribbon(aes(x = EventDate, ymin = loCI, ymax = upCI), data = ma7_df, alpha = 0.4) +
  scale_fill_manual(name="", values=c("#FF7000", "#00A3FF", 
                                      "#813800", "#00588B"),
                    labels=c("Weekday Anticipated", "Weekend Anticipated", 
                             "Weekday Reported", "Weekend Reported")) +
  scale_x_date(expand=c(0,0), date_breaks = "2 week", date_labels = "%b %d",
               limits = c(ymd("2020-02-29"), nowcast_date+ddays(1))) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, max(pred_df$upCI) + 100)) +
  theme_bw() +
  theme(legend.position = "top", 
        plot.margin = margin(10, 30, 10, 10), 
        legend.text = element_text(size = 12),
        axis.text = element_text(size = 10), 
        axis.title = element_text(size = 15), 
        plot.caption = element_text(size = 12)) +
  labs(x = "Date", y = "Number of cases",
       caption = paste0("Data updated as of ", dat_date))


#### Export graphmakers
ma7_df <- rename(ma7_df, ma7_mean = mean, ma7_loCI = loCI, ma7_upCI = upCI)
pred_df_comp <- ma7_df %>%
  left_join(pred_df)
write_csv(pred_df_comp, "testApp/data/statewide-nowcast-preds.csv")
