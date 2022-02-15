library(statnet)

load(url("https://acaimo.github.io/teaching/data/lazega.Rdata"))
y <- lazega$ADVICE
cp <- hcl.colors(2, "Red-Blue") # color palette

set.seed(7)
par(mar = rep(1, 4)) 
plot(y, 
     mode = "kamadakawai",
     edge.col = "grey",
     vertex.cex = log(y %v% "seniority") + 0.5,
     vertex.col = cp[y %v% "gender"])

legend("topright", 
       pt.bg  = cp[sort(unique(y %v% "gender"))], 
       pt.cex = 1.2,
       pch    = 21, 
       legend = c("Man", "Woman"), 
       title  = 'Gender')



#MODELING TASK
#Estimate an exponential random graph model including the 
#density effect and the homophily effect based on gender and 
#practice. Interpret the results obtained. Perform a goodness-of-fit
#diagnostic check.

ergmodel_1 <- y ~ edges + nodematch('gender') + nodematch('practice')
mle.ergmodel_1 <- ergm(ergmodel_1)
summary(mle.ergmodel_1)

set.seed(1)
y_sim_1 <- simulate(mle.ergmodel_1, # estimated model
                    nsim = 1)     # number of networks to simulate

set.seed(1)
par(mar = rep(0, 4))
plot.network(y_sim_1, 
             vertex.cex = 2)

gof(mle.ergmodel_1)
