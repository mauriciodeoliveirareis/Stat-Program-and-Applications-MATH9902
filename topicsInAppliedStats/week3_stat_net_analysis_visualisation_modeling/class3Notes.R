library(statnet)
data(zach)
y <- zach

rgmodel <- y ~ edges 

mle.rgmodel <- ergm(rgmodel)
summary(mle.rgmodel)

# estimate model including density and homofily effect
ergmodel_1 <- y ~ edges + nodematch('faction') # faction = x nodal attribute

mle.ergmodel_1 <- ergm(ergmodel_1)

summary(mle.ergmodel_1)
summary(ergmodel_1)


#estimate model by nnodal degree (B model)
ergmodel_2 <- y ~ sociality # sociality = nodal degree

mle.ergmodel_2 <- ergm(ergmodel_2)

summary(mle.ergmodel_2)


#Model simulation plot
set.seed(1)
y_sim_2 <- simulate(mle.ergmodel_2, # estimated model
                    nsim = 1)     # number of networks to simulate

set.seed(1)
par(mar = rep(0, 4))
plot.network(y_sim_2, 
             vertex.cex = 2)

#goodness of fit
gof(mle.ergmodel_1)
