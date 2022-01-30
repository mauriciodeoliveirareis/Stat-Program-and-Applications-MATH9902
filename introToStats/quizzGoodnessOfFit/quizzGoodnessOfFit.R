mu <- 100
sigma <- 15

qchisq(0.95,3)

observedData <- c(82,88, 111, 113, 108, 99, 97, 99, 95, 106, 101, 104, 102, 100, 91, 98)

x_bar <- mean(observedData)

#expected value < 90 
16*pnorm(90,100,15)