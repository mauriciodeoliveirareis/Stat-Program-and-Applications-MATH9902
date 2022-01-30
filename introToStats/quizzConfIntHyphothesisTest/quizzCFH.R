#sleep patterns 
mu <- 8
x_bar <- 10
s <- 2
n <- 40
# Two sided 99% Z 2.58
z <- (x_bar - mu) / (s / sqrt(n))

x_bar - 2.58 * (s / (sqrt(n)))

#Santa survey 
n <- 100
p_zero <- 0.9 #historical 
p_hat <- 0.8 #survey 
#one sided 95% z = 1.69
z <- (p_hat - p_zero) / sqrt((p_zero * (1 - p_zero) / n))
