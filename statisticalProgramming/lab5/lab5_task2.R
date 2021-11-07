calculatePoisson <- function(l,u,lambda, printPlots = FALSE) {
  if (lambda <=0) stop("lambda must be positive")
  
  x <- l:u
  p_x <- dpois(x, lambda = lambda)
  #TODO Take a look into this, cdf is certanly not well calculated
  F_x <- ppois(x, lambda = lambda)
  
  cdf <- stepfun(x, c(0, F_x))
  
  if(printPlots) {
    X <- data.frame(x, p_x, F_x)
    
    par(mfrow = c(1, 2))
    
    plot(x, p_x, lwd = 3, type = 'h', main = 'pmf of X', ylab = 'p(x)')
    
    
    plot.stepfun(cdf, verticals = FALSE, do.points = TRUE, 
                 pch = 16, lwd = 3,
                 main = 'cdf of X',
                 ylab = 'F(x)')
  }
  
  list(cdf = cdf,
       F_x = F_x,
       p_x = p_x)
}

calculatePoisson(0,10,5, TRUE)