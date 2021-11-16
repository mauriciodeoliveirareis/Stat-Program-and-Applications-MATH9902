continuousPdfAndCdf <- function(lambda, x1, printPlots = FALSE) {
  if (x1 < 0) stop("x1 must be positive")
  
  f_x <- function(x) lambda * exp(-1 * lambda * x)
  f_x_for_x1 <- integrate(f_x, lower = 0, upper = x1)
  
  E_X <- integrate(f_x, lower = 0, upper = Inf)$value
  x2.f_x <- function(x) lambda * exp(-1 * lambda * x^2)
  E_X2 <- integrate(x2.f_x, lower = 0, upper = 2)$value
  Var_X <- E_X2 - E_X^2
  
  if(printPlots) {
    curve(f_x, from = 0, to = 100, ylab = 'f(x)', 
          main = 'pdf of X', lwd = 3)
  }
  
  list(E_X = E_X,
       Var_X = Var_X,
       f_x_for_x1 = f_x_for_x1)
  
}

continuousPdfAndCdf(0.1,2, TRUE)
