carsDf <- read.csv("/Users/mauricio.reis/appliedStatistics/Stat-Program-and-Applications-MATH9902/introToStats/quizzCars/MYcars.csv")

meanSpeed <- mean(carsDf$speed)
sdSpeed <- sd(carsDf$speed)

interquartileRange <- IQR(carsDf$dist)

numRows <- nrow(carsDf)
