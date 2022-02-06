#install.packages("statnet")
library(statnet)

data(florentine)
?florentine

heatmap(x = flobusiness[, ],      # adjacency matrix of the flobusiness network
        Rowv = NA, Colv = "Rowv", # no dendrograms
        col = c(0, 1),            # black and white
        symm = TRUE,              # indicating if x should be treated symmetrically; 
        # it can only be true when x is a square matrix.
        frame = TRUE)

set.seed(18)
par(mar = rep(1, 4)) # plot margins
plot.network(flobusiness, 
             label = network.vertex.names(flobusiness), # family names
             vertex.cex = 2)


summary(flobusiness, 
        print.adj = F)


#a random graph
n <- 14 
p <- 0.1 

g <- rgraph(n = n,            # number of nodes in the network
            m = 1,            # number of networks to generate
            tprob = p,        # fixed tie probability (Bernoulli parameter p)
            mode = 'digraph') # directed network (use "graph" for undirected networks)
summary(g)


#a random graph with different probabily for each dyad 
set.seed(1)

p_ij <- rbeta(n * n, shape1 = 2, shape2 = 5) 

g <- rgraph(n = n,          
            m = 1,          
            tprob = p_ij,   
            mode = 'digraph')

heatmap(g,
        Rowv = NA, Colv = "Rowv", 
        symm = TRUE, 
        col = c(0, 1), 
        frame = TRUE)


plot(as.network(g),
     label = 1:n, 
     vertex.cex = 2)