ConditionalDieProbablity <- function(d1, s) {
  if (d1 <  1 | d1 > 6 ) stop("d1 must be between 1 and 6")
  if (s <  2 | s > 12 ) stop("s must be between 2 and 12")
  
  die <- 1:6
  rolls <- expand.grid(die, die) # sample space of two dice
  R <- data.frame(rolls, rowSums(rolls))
  colnames(R) <- c("D_1", "D_2", "S")
  
  #If d1 5 and s = 7
  #P(d1 = 5 ∩ s = 7) / P(S = 7)
  sum(R$D_1 == d1 & R$S == s) / sum(R$S == s)
  
  
  
}

set.seed(7)
condProb <- ConditionalDieProbablity(5,11)
#TODO: This doesn't seem right, I was expecting 1/6 or 0 here
condProb
