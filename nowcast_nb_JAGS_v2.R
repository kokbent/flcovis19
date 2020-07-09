code <- nimbleCode({
  for (i in 1:N) {
    mu[i] <- exp(mu0 + alpha[ed[i]] + beta[rd[i]] + 
                   # gamma[ed[i], rd[i]] + 
                   eta[rd[i], repl[i]] + delta*hol[i])
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
  
  # gamma: ED and RD interaction
  # for (k in 1:NRD) {
  #   gamma[1,k] <- 0
  #   for (j in 2:NED) {
  #     gamma[j,k] ~ dnorm(gamma[j-1,k], tau_g)
  #   }
  # }
  
  # eta: Replicate adjustment
  for (k in 1:NRD) {
    eta[k,1] <- 0
    for (l in 2:(NREPL - k + 1)) {
      eta[k,l] ~ dnorm(eta[k,l-1], tau_e)
    }
    for (l in (NREPL - k + 2):NREPL) {
      eta[k,l] <- 0
    }
  }
  
  # delta: Covariate effect
  # currently control holiday effect
  delta ~ dnorm(0, 1/100)
  
  # phi: Overdispersion
  phi ~ dunif(0, 1000)
  
  # tau_a: Precision of alpha-
  sig_a ~ T(dnorm(0, .5), 0, )
  tau_a <- 1/(sig_a * sig_a)
  
  # tau_b: Precision of beta
  sig_b ~ T(dnorm(0, .5), 0, )
  tau_b <- 1/(sig_b * sig_b)
  
  # tau_g: Precision of gamma
  # sig_g ~ T(dnorm(0, 0.1), 0, )
  # tau_g <- 1/(sig_g * sig_g)
  
  # tau_e: Precision of eta
  sig_e ~ T(dnorm(0, .1), 0, )
  tau_e <- 1/(sig_e * sig_e)
})
