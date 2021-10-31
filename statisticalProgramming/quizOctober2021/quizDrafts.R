printPlots <- function(x, p_x, cdf) {
  par(mfrow = c(1, 2))
  
  plot(x, p_x, lwd = 3, type = 'h', main = 'pmf of X', ylab = 'p(x)')
  
  plot.stepfun(cdf, verticals = FALSE, do.points = TRUE, 
               pch = 16, lwd = 3,
               main = 'cdf of X',
               ylab = 'F(x)')
}

calculateFeaturesOfUniformDistribution <- function(x = 1:5, p_x = rep(0.2, 5), printPlots = FALSE) {
  if (length(x) != length(p_x)) stop("Size of x and p_x vector must be the same")
  
  F_x <- cumsum(p_x)
  sumOfAllProbabilities <- tail(F_x, n=1)  
  if(sumOfAllProbabilities != 1) stop("Probabilities must add up to 1")
  
  cdf <- stepfun(x, c(0, F_x))
  
  if(printPlots) {
    printPlots(x, p_x, cdf)
  }
  
  E_X <- c(x %*% p_x) 
  Var_X <- c(x - E_X)^2 %*% p_x
  
  list(E_X = E_X,
       Var_X = Var_X,
       F_x = F_x,
       cdf = cdf)
}

#Task 2
t2_px <- c(0.22,0.08,0.2,0.12,0.38)
x <- c(1:5)
t2_result<- calculateFeaturesOfUniformDistribution(x, t2_px, TRUE)
t2_result$cdf(2)
t2_px[4] + t2_px[5]
t2_px[4] + t2_px[5]
1 - t2_px[2] #4
1 - (t2_px[2] + t2_px[3]) #5
1 - (t2_px[2]) + t2_px[3] - t2_px[3] #6
t2_px[3] / (1 - t2_px[1]) #7  
0/t2_px[3] #8
t2_result$cdf(2) + t2_px[5] #9

probXSmallerThan3 <- t2_result$cdf(3)
probXSmallerThan2OrXIs4 <- t2_result$cdf(2) + t2_px[4]
probXEqualTo3GivenXSmallerOrEqualTo4 <- t2_px[3] / t2_result$cdf(4)

print(t2_result)
print(probXSmallerThan3)
print(probXSmallerThan2OrXIs4)
print(probXEqualTo3GivenXSmallerThan4)
