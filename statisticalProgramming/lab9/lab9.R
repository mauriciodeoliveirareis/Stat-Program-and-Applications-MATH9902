conf_int <- function(n = 100,
                     alpha = 0.05,
                     mu = 0,
                     sigma2 = 1) {
  #TODO validations here  
  set.seed(19)
  sample <- rnorm(n, mu, sigma)
  xbar <- mean(sample)        # estimate of mu
  se <- sd(sample) / sqrt(n)  # standard error of xbar
  
  L <- xbar - qt(1 - alpha / 2, n - 1) * se  # CI lower limit
  U <- xbar + qt(1 - alpha / 2, n - 1) * se  # CI upper limit
  CI_mu <- c(L, U)
  
  s2 <- var(sample) # estimate of sigma^2
  
  s2L <- ((n - 1) * s2) / qchisq(1 - alpha / 2, n - 1)  # CI lower limit
  s2U <- ((n - 1) * s2) / qchisq(alpha / 2, n - 1)      # CI upper limit
  CI_sigma2 <- c(s2L, s2U)
  list(xbar = xbar,
       CI_mu = CI_mu,
       CI_sigma2 = CI_sigma2)
} 

conf_int()