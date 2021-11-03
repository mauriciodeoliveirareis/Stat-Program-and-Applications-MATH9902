calculateBinomial <- function(n, p, printPlots = FALSE) {
  if (n < 1) stop("n must be positive")
  if (n %% 1 != 0) stop("n must be integer")
  if (p < 0 | p > 1) stop("p must be between 0 and 1")
  
  x <- 0:n 
  p_x <- dbinom(x,           
                size = n,    
                prob = p)    
  
  F_x <- pbinom(x, size = n, prob = p) 
  
  E_X <- n * p
  Var_X <- E_X*(1-p)
  
  if(printPlots) {
    X <- data.frame(x, p_x, F_x)
    
    par(mfrow = c(1, 2))
    
    plot(x, p_x, lwd = 3, type = 'h', main = 'pmf of X', ylab = 'p(x)')
    abline(v=E_X, col="purple")
    abline(v=E_X-Var_X, col="blue")
    abline(v=E_X+Var_X, col="red")
    cdf <- stepfun(x, c(0, F_x))
    plot.stepfun(cdf, verticals = FALSE, do.points = TRUE, 
                 pch = 16, lwd = 3,
                 main = 'cdf of X',
                 ylab = 'F(x)')
  }
  
  list(E_X = E_X,
       Var_X = Var_X,
       F_x = F_x,
       p_x = p_x)
}
calculateBinomial(-1,1) #triggers "n must be positive"
calculateBinomial(3.14,1) #triggers "n must be integer"
calculateBinomial(-1,14) "p must be between 0 and 1"

calculateBinomial(10,0.5, TRUE)