Urn_draws <- function(ndraws = 1, nOfBallsOnUrn = 100, invalidNumberOfDrawsMessage = 'ndraws cannot be > nOfBallsOnUrn!') {
  
  if (ndraws > nOfBallsOnUrn) stop(invalidNumberOfDrawsMessage)
  
  balls <- sample(1:nOfBallsOnUrn, size = ndraws, replace = FALSE)
  for (ball in balls) {
    if(ball < 10) print(paste(ball, '< 10'))
  }

  mean <- mean(balls)
  
  min <- min(balls)
  
  max <- max(balls)
  
  even_prop <- sum((balls %% 2) == 0) / ndraws
  
  sortedBalls <- sort(balls)
  
  list(meanzz = mean,
       min = min,
       max = max,
       even_prop = even_prop,
       sortedBalls = sortedBalls)
}

set.seed(7)
UD <- Urn_draws(150, 151)
UD
