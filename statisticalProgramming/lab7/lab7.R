calculateConditionalNormal <- function(x1=-0.5, x2=0.5, mu = 0, sigmaSq = 1, printPlots = FALSE) {
  if (x1 >= x2) stop("x1 must be smaller than x2")
  sigma = sqrt(sigmaSq)
  
  if(printPlots) {
    par(mfrow = c(1, 2))
    
    curve(dnorm(x, mean = mu, sd = sigma), 
          from = mu - 3 * sigma, to = mu + 3 * sigma, 
          ylab = 'f(x)', main = 'pdf of X', lwd = 3)
    
    curve(pnorm(x, mean = mu, sd = sigma), 
          from = mu - 3 * sigma, to = mu + 3 * sigma, 
          ylab = 'F(x)', main = 'cdf of X', lwd = 3)
  }
  
  xSmallerThanX2 <- pnorm(x2, mu, sigma)
  xSmallerThanX1 <- pnorm(x1, mu, sigma)
  xSmallerThanX1GivenX2 = (xSmallerThanX1 * xSmallerThanX2) / xSmallerThanX2
  xSmallerThanX1GivenX2
}

calculateConditionalNormal(2,1) #x1 must be smaller than x2

calculateConditionalNormal(printPlots = TRUE)