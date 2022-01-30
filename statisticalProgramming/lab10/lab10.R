my.t.test <- function(n = 57,
                     mu = 22,
                     sigma2 = 4,
                     alpha = 0.05,
                     mu_0 = 23) {
  #if( n < 100 | n > 10000 ) stop("n must be between 100 and 10,000")
  if( alpha <= 0 | alpha >= 1 ) stop("alpha must be between 0 and 1")
  
  sigma <- sqrt(sigma2)
  set.seed(7)
  sample <- rnorm(n, mu, sigma)
  x_bar <- mean(sample)
  s <- sd(sample)
  r_t.test <- t.test(sample, mu = mu_0, alternative = 'two.sided', conf.level = 1-alpha)
  #TODO this formula is for Z test, discove how to do a t.test and consider conf interval  
  z <- (x_bar - mu_0)/ (s/sqrt(n))
  
  d_limit <- x_bar + (z * (s / sqrt(n)))
  u_limit <- x_bar - (z * (s / sqrt(n)))
  list(x_bar = x_bar, 
       s = s,
       r_t.test = r_t.test,
       z = z)
}

#my.t.test(n=1) #fails as n must be at least 100
#my.t.test(alpha=1.1) #fails as alpha can't be bigger than 1

my.t.test()


