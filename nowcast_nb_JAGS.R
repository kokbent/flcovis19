code <- nimbleCode({
  for (i in 1:N) {
    lc[i] <- mu + alpha[ed[i]] + beta[rd[i]] + gamma[ed[i], rd[i]] + delta*hol[i]
    y[i] ~ dnegbin(1/(exp(lc[i])/r+1), r)
  }
  
  # mu: Grand mean
  mu ~ dnorm(0, 1/10)
  
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
  
  # gamma: ED and RD interaction
  for (k in 1:NRD) {
    gamma[1,k] <- 0
    for (j in 2:NED) {
      gamma[j,k] ~ dnorm(gamma[j-1,k], tau_g)
    }
  }
  
  # delta: Covariate effect
  # currently control holiday effect
  delta ~ dnorm(0, 1)
  
  # r: Overdispersion
  r ~ dunif(0, 100)
  
  # tau_a: Precision of alpha
  sig_a ~ T(dnorm(0, 0.1), 0, )
  tau_a <- 1/(sig_a * sig_a)
  
  # tau_b: Precision of beta
  sig_b ~ T(dnorm(0, 1), 0, )
  tau_b <- 1/(sig_b * sig_b)
  
  # tau_g: Precision of gamma
  sig_g ~ T(dnorm(0, 0.1), 0, )
  tau_g <- 1/(sig_g * sig_g)
})
