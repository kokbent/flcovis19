library(tidyverse)
library(lubridate)
library(stringr)
library(nimble)
library(googlesheets4)

if (packageVersion("googlesheets4") == "0.2.0") {
  gs4_auth("kokbent@gmail.com")
} else {
  sheets_auth("kokbent@gmail.com")
}

dat <- sheets_read("1EUr3mhs1PnN4HrF4HgYH1EalQwOgH1nwhHddpZ-fHJg", 
                   sheet = "Reporting delay of death")

colnames(dat)[1] <- "EventDate"
dat$EventDate <- as.Date(dat$EventDate)
dat <- as.data.frame(dat)
for (i in 1:nrow(dat)) {
  v <- as.matrix(dat[i,2:ncol(dat)])
  max_wval <- which(!is.na(v)) %>% max
  ind <- which(is.na(v))
  ind <- ind[ind > max_wval]
  v[ind] <- v[max_wval]
  dat[i,2:ncol(dat)] <- v
}

dat_long <- dat %>%
  pivot_longer(-EventDate, 
               names_to = "ChartDate",
               values_to = "n")
dat_long$ChartDate <- as.Date(dat_long$ChartDate)

nowcast_date <- max(dat_long$ChartDate)
start_date <- nowcast_date - ddays(27)
end_date <- nowcast_date
dt_range <- seq(end_date, start_date, by = -1)

dat_train <- data.frame()
for (i in 1:length(dt_range)) {
  dt <- dt_range[i]
  df <- dat_long %>%
    filter(EventDate == dt)
  df$ret_day <- as.numeric(df$ChartDate - df$EventDate)
  df1 <- df %>% 
    filter(ret_day >= 0, ret_day <= 21) %>%
    mutate(n = ifelse(is.na(n), 0, n))
  df1$ndiff <- c(0, diff(df1$n))
  df1 <- df1 %>%
    filter(ret_day > 0,
           EventDate + ret_day <= nowcast_date)
  
  dat_train <- rbind(dat_train, df1)
}

dat_train$rt_sat <- wday(dat_train$ChartDate, label = T) == "Sun"
dat_train$rt_sun <- wday(dat_train$ChartDate, label = T) == "Mon"
dat_train$EventDate2 <- as.numeric(dat_train$EventDate - start_date) + 1
dat_train$ndiff <- ifelse(dat_train$ndiff < 0, 0, dat_train$ndiff)

#### Fit with NIMBLE ----
constants <- list(N = nrow(dat_train),
                  NED = max(dat_train$EventDate2),
                  NRD = max(dat_train$ret_day),
                  ed = dat_train$EventDate2,
                  rd = dat_train$ret_day)

data <- list(y = dat_train$ndiff,
             rt_sat = as.numeric(dat_train$rt_sat),
             rt_sun = as.numeric(dat_train$rt_sun))
inits <- list(alpha = rep(0, constants$NED), beta = rep(0, constants$NRD), 
              delta_sat = 0, delta_sun = 0, phi = 5)

source("nowcast_noeta_JAGS.R")
system.time(model <- nimbleModel(code, constants = constants, data = data, inits = inits,
                                 calculate = T))

conf <- configureMCMC(model, 
                      monitors = c("mu0", "alpha", "beta", "phi", "delta_sat", "delta_sun"))
mcmc <- buildMCMC(conf)
modelc <- compileNimble(model)
mcmcc <- compileNimble(mcmc)
# Uncomment for quick sanity check
# samp <- runMCMC(mcmcc, niter = 10000, nburnin = 2500, thin = 1, nchains = 3,
#                 progressBar = T, samples = T, samplesAsCodaMCMC = T)
samp <- runMCMC(mcmcc, niter = 20000, nburnin = 10000, thin = 10, nchains = 3,
                setSeed = c(4326, 4327, 4328), progressBar = T,
                samples = T, samplesAsCodaMCMC = T)

# for (i in 1:3) {
#   ind <- which(colMeans(is.na(samp[[i]])) == 0)
#   samp[[i]] <- samp[[i]][,ind]
# }

# summ <- MCMCvis::MCMCsummary(samp)


#### Extract Posterior Samples
pos <- samp
pos <- do.call(rbind, samp)
pos_mu <- pos[,"mu0"]
pos_alpha <- pos[,str_detect(colnames(pos), "alpha")]
pos_beta <- pos[,str_detect(colnames(pos), "beta")]
pos_delta_sat <- pos[,str_detect(colnames(pos), "delta_sat")]
pos_delta_sun <- pos[,str_detect(colnames(pos), "delta_sun")]
pos_phi <- pos[,"phi"]
n_pos <- length(pos_phi)

#### Test set
dat_test <- data.frame(EventDate = seq(nowcast_date - ddays(20), nowcast_date - ddays(1), by = 1),
                       ret_day = 21:2)
dat_test$EventDate2 <- as.numeric(dat_test$EventDate - start_date) + 1

dat_test1 <- data.frame()
for (i in 1:nrow(dat_test)) {
  df <- data.frame(EventDate = dat_test$EventDate[i],
                   EventDate2 = dat_test$EventDate2[i],
                   ret_day = dat_test$ret_day[i]:21,
                   ChartDate = dat_test$EventDate[i] + ddays(dat_test$ret_day[i]:21))
  df$rt_sat <- wday(df$ChartDate, label = T) == "Sun"
  df$rt_sun <- wday(df$ChartDate, label = T) == "Mon"
  dat_test1 <- rbind(dat_test1, df)
}

dat_test1$rt_sat <- wday(dat_test1$ChartDate, label = T) == "Sun"
dat_test1$rt_sun <- wday(dat_test1$ChartDate, label = T) == "Mon"

#### Extract predictions
pred_mat <- matrix(NA, nrow = nrow(dat_test1), ncol = n_pos)
for (j in 1:nrow(dat_test1)) {
  ind_ED <- dat_test1$EventDate2[j]
  ind_RD <- dat_test1$ret_day[j]
  rt_sat <- dat_test1$rt_sat[j]
  rt_sun <- dat_test1$rt_sun[j]
  
  # Method 1
  m <- exp(pos_mu + pos_alpha[,ind_ED] + pos_beta[,ind_RD] +
             pos_delta_sat * rt_sat + pos_delta_sun * rt_sun)
  
  pred_mat[j,] <- rnbinom(n_pos,
                          size = pos_phi,
                          mu = m)
}

for (j in 1:nrow(dat_test)) {
  cond <- dat_test1$EventDate == dat_test$EventDate[j]
  preds <- colSums(pred_mat[cond,,drop=F])
  dat_test$mean_pred[j] <- mean(preds)
  dat_test$loCI[j] <- quantile(preds, 0.025)
  dat_test$upCI[j] <- quantile(preds, 0.975)
}
#### Carpentry for visualization
## date_train is restricted to 14-day window for model training
## But in actual visualization, we show actual number of cases reported up to the
## nowcast date, i.e. Case counts for EventDate outside of 14-day window should 
## be larger than those derived from date_train.
case_actual <- dat_long %>%
  filter(ChartDate == nowcast_date) %>%
  filter(!is.na(n))

# case_train <- dat_train %>%
#   group_by(EventDate) %>%
#   summarise(n=sum(n))

## Adding predictions and pivot table in case_actual
case_actual <- case_actual %>%
  left_join(dat_test %>% select(EventDate, mean_pred, loCI, upCI))

case_actual_long <- case_actual %>%
  select(EventDate, n, pred = mean_pred) %>%
  pivot_longer(-EventDate,
               names_to = "type",
               values_to = "n") %>%
  filter(!is.na(n))

case_actual_long$type <- factor(case_actual_long$type, levels = c("pred", "n"))

## Export
write_csv(case_actual, "testApp/data/death_preds.csv")

#### Graphing
# ggplot() +
#   geom_col(aes(x=EventDate, y=n, fill=type), data=case_actual_long) +
#   # geom_point(aes(x=EventDate, y=n), data=truth) +
#   geom_errorbar(aes(x=EventDate, ymin=n+loCI, ymax=n+upCI), data=case_actual, width=0.25) +
#   scale_fill_manual(name="", values=c("#d55e00", "#0072b2"),
#                     labels=c("Anticipated", "Reported")) +
#   scale_x_date(expand=c(0,0), date_breaks = "2 week", date_labels = "%b %d",
#                limits = c(ymd("2020-05-13"), nowcast_date)) +
#   scale_y_continuous(expand = c(0, 0), 
#                      limits = c(0, max(case_actual$n + case_actual$upCI, na.rm = T) * 1.1)) +
#   theme_bw() +
#   theme(legend.position = "top", 
#         plot.margin = margin(10, 30, 10, 10), 
#         legend.text = element_text(size = 12),
#         axis.text = element_text(size = 10), 
#         axis.title = element_text(size = 15), 
#         plot.caption = element_text(size = 12)) +
#   labs(x = "", y = "",
#        caption = paste0("Data updated as of ", nowcast_date),
#        title = "Number of COVID-19 death by date of death")

