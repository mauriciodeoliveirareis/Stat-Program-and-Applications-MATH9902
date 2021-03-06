explainRandomSample <- function(n, mu, sigmaSq, x1) {
  if (n < 100 | n > 10000) stop("n must be between 100 and 10,000")
  sigma <- sqrt(sigmaSq)
  sample <- rnorm(n,mu,sigma)
  xbar <- mean(sample)
  s <- sd(sample)
  se <- s/sqrt(n)
  x1CdfSample <- pnorm(x1, xbar, s)
  x1CdfPopulation <- pnorm(x1, mu, sigma)
  F_x1 <- sum(sample < x1) / n
  list(xbar = xbar,
       s = s,
       se = se, 
       x1CdfSample = x1CdfSample,
       x1CdfPopulation = x1CdfPopulation,
       F_x1 = F_x1)
}
set.seed(1) 
#explainRandomSample(1,1,1,1) #n must be between 100 and 10,000
#explainRandomSample(999999,1,1,1) # n must be between 100 and 10,000
explainRandomSample(200,22,4,26)
