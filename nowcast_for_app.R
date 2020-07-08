library(tidyverse)
library(lubridate)
library(stringr)
library(nimble)

folder <- "../flovid19-data/linelist/"

dating <- function (dt) {
  if (is.numeric(dt)) {
    return(
      as.POSIXct(dt/1000, tz = "GMT", origin = "1970/01/01") %>% 
        as.Date()
    )
  } else {
    return(ymd_hms(dt) %>% as.Date)
  }
}

nowcast_date <- Sys.Date() - ddays(2)
start_date <- nowcast_date - ddays(21)
end_date <- nowcast_date + ddays(2)
dt_range <- seq(end_date, start_date, by = -1)

dat_train <- data.frame()
for (i in 1:length(dt_range)) {
  dt <- dt_range[i]
  in_file <- paste0(folder, "linelist_", str_remove_all(dt, "-"), ".csv")
  
  ll <- read.csv(in_file) %>%
    mutate(EventDate = dating(EventDate),
           ChartDate = dating(ChartDate),
           ret_day = as.numeric(ChartDate - EventDate))
  
  df <- ll %>%
    filter(EventDate >= start_date, EventDate <= nowcast_date) %>%
    filter(ret_day >= 0, ret_day <= 14) %>%
    # filter(ret_day <= as.numeric(nowcast_date - EventDate)) %>%
    group_by(EventDate, ret_day) %>%
    summarise(n=n())
  
  df <- df %>%
    ungroup() %>%
    complete(EventDate = seq(start_date, nowcast_date, by = 1),
             ret_day = 0:14,
             fill = list(n = 0)) %>%
    filter(ret_day <= as.numeric(nowcast_date - EventDate))
  
  df$ReportDate <- dt - ddays(2)
  df$repl <- as.numeric(df$ReportDate - df$EventDate + 1 - df$ret_day)
  df <- df %>%
    filter(repl > 0)
  
  dat_train <- rbind(dat_train, df)
}

dat_train <- dat_train %>%
  group_by(ret_day, repl) %>%
  mutate(nrep = n()) %>%
  filter(nrep >= 3) %>%
  ungroup

dat_train$EventDate2 <- as.numeric(dat_train$EventDate - start_date) + 1
dat_train$hol <- wday(dat_train$EventDate, label = T) %in% c("Sat", "Sun")

#### Fit with NIMBLE ----
constants <- list(N = nrow(dat_train),
                  NED = max(dat_train$EventDate2),
                  NRD = max(dat_train$ret_day) + 1,
                  NREPL = max(dat_train$repl),
                  ed = dat_train$EventDate2,
                  rd = dat_train$ret_day + 1,
                  repl = dat_train$repl)

data <- list(y = dat_train$n,
             hol = as.numeric(dat_train$hol))
inits <- list(alpha = rep(0, constants$NED), 
              beta = rep(0, constants$NRD), 
              delta = 0,
              phi = 5,
              eta = matrix(0, constants$NRD, constants$NREPL))

source("nowcast_nb_JAGS_v2.R")
system.time(model <- nimbleModel(code, constants = constants, data = data, inits = inits,
                                 calculate = T))

conf <- configureMCMC(model, 
                      monitors = c("mu0", "alpha", "beta", 
                                   "eta", "phi", "delta"))
mcmc <- buildMCMC(conf)
modelc <- compileNimble(model)
mcmcc <- compileNimble(mcmc)
# Uncomment for quick sanity check
# samp <- runMCMC(mcmcc, niter = 5000, nburnin = 2500, thin = 1, nchains = 3,
#                 progressBar = T, samples = T, samplesAsCodaMCMC = T)
samp <- runMCMC(mcmcc, niter = 100000, nburnin = 50000, thin = 10, nchains = 1,
                setSeed = 4342024, progressBar = T,
                samples = T, samplesAsCodaMCMC = T)

# summ <- MCMCvis::MCMCsummary(samp)


#### Extract Posterior Samples
# pos <- do.call(rbind, samp)
pos <- samp
pos_mu <- pos[,"mu0"]
pos_alpha <- pos[,str_detect(colnames(pos), "alpha")]
pos_beta <- pos[,str_detect(colnames(pos), "beta")]
pos_eta <- pos[,str_detect(colnames(pos), "^eta")]
pos_delta <- pos[,str_detect(colnames(pos), "delta")]
pos_phi <- pos[,"phi"]
n_pos <- length(pos_phi)

#### Test set
case_rt_test <- data.frame(EventDate = seq(nowcast_date - ddays(13), nowcast_date, by = 1),
                           ret_day = 14:1)
case_rt_test$EventDate2 <- as.numeric(case_rt_test$EventDate - start_date) + 1
case_rt_test$hol <- wday(case_rt_test$EventDate, label = T) %in% c("Sat", "Sun")


#### Extract predictions
pred_mat <- matrix(NA, n_pos, nrow(case_rt_test))
for (j in 1:nrow(case_rt_test)) {
  ind_ED <- case_rt_test$EventDate2[j]
  ind_RD <- case_rt_test$ret_day[j]
  hol <- case_rt_test$hol[j]
  
  # Find eta index
  rds <- 1:15
  repl <- 16 - rds
  # repl <- ifelse(repl > 15, 15, repl)
  
  eta_colnames <- paste0("eta[", rds, ", ", repl, "]")
  m <- exp(pos_mu + pos_alpha[,ind_ED] + pos_beta[,rds] +
             pos_eta[,eta_colnames] + pos_delta * hol)
  
  preds <- rnbinom(n_pos*15,
                   size = pos_phi,
                   mu = m) %>%
    matrix(n_pos)
  
  tmp_dat <- dat_train %>%
    filter(EventDate2 == ind_ED) %>%
    filter(ReportDate == nowcast_date)
  
  rds1 <- 1:ind_RD
  repl1 <- ind_RD:1
  
  eta_colnames1 <- paste0("eta[", rds1, ", ", repl1, "]")
  diffs <- exp(pos_eta[,eta_colnames[rds1]] - pos_eta[,eta_colnames1])
  
  if (length(tmp_dat$n) == 1) {
    adj_n <- diffs * tmp_dat$n
  } else {
    adj_n <- diffs %*% tmp_dat$n
  }
  
  preds <- cbind(adj_n, preds[,-rds1])
  
  pred_mat[,j] <- rowSums(preds)
}

#### Carpentry for visualization
dt <- dt_range[1]
in_file <- paste0(folder, "linelist_", str_remove_all(dt, "-"), ".csv")
ll <- read.csv(in_file) %>%
  mutate(EventDate = dating(EventDate),
         ChartDate = dating(ChartDate),
         ret_day = as.numeric(ChartDate - EventDate))

case_actual <- ll %>%
  filter(EventDate >= ymd("2020-03-01"), EventDate <= nowcast_date) %>%
  filter(ret_day >= 0) %>%
  group_by(EventDate) %>%
  summarise(n=n())

case_train <- dat_train %>%
  group_by(EventDate) %>%
  summarise(n=sum(n))

## Adding predictions and pivot table in case_actual
case_actual$pred <- NA
ind <- nrow(case_actual) - nrow(case_rt_test) + 1
case_actual$pred[ind:nrow(case_actual)] <- colMeans(pred_mat)
case_actual$pred <- case_actual$pred - case_actual$n

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

## 7-day Moving Average
ma7_mat <- matrix(NA, n_pos, nrow(case_actual))
for (i in 1:n_pos) {
  ts_n <- c(case_actual$n[1:(nrow(case_actual) - nrow(case_rt_test))],
            pred_mat[i,])
  ma7_mat[i,] <- stats::filter(ts_n, rep(1/7, 7), sides = 2)
  
  # Exceptions for the tail
  ma7_mat[i, nrow(case_actual) - 2] <- mean(ts_n[(nrow(case_actual)-5):nrow(case_actual)])
  ma7_mat[i, nrow(case_actual) - 1] <- mean(ts_n[(nrow(case_actual)-4):nrow(case_actual)])
  ma7_mat[i, nrow(case_actual)] <- mean(ts_n[(nrow(case_actual)-3):nrow(case_actual)])
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
           alpha = 0.6) +
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
  labs(x = "Date", y = "Number of reported cases",
       caption = paste0("Data updated as of ", nowcast_date + ddays(2)))


#### Export graphmakers
ma7_df <- rename(ma7_df, ma7_mean = mean, ma7_loCI = loCI, ma7_upCI = upCI)
pred_df_comp <- ma7_df %>%
  left_join(pred_df)
write_csv(pred_df_comp, "testApp/data/statewide-nowcast-preds.csv")
