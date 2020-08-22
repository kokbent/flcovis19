code <- nimbleCode({
  for (i in 1:N) {
    mu[i] <- exp(mu0 + alpha[ed[i]] + beta[rd[i]] + 
                   delta_sat * rt_sat[i] + delta_sun * rt_sun[i])
    y[i] ~ dnegbin(phi/(mu[i] + phi), phi)
  }
  
  # mu0: Grand mean
  mu0 ~ dnorm(0, 1/100)
  
  # alpha: Event Day effect
  alpha[1] <- 0
  for (j in 2:NED) {
    alpha[j] ~ dnorm(alpha[j-1], tau_a)
  }
  
  # beta: Reporting Day effect
  beta[1] <- 0
  for (k in 2:NRD) {
    beta[k] ~ dnorm(beta[k-1], tau_b)
  }
  
  # delta: Covariate effect
  # currently control holiday effect
  delta_sat ~ dnorm(0, 0.1)
  delta_sun ~ dnorm(0, 0.1)
  # delta <- 0
  
  # phi: Overdispersion
  phi ~ dunif(0, 10000)
  
  # tau_a: Precision of alpha
  sig_a ~ T(dnorm(0, 0.5), 0, )
  tau_a <- 1/(sig_a * sig_a)
  
  # tau_b: Precision of beta
  sig_b ~ T(dnorm(0, 0.5), 0, )
  tau_b <- 1/(sig_b * sig_b)
})
