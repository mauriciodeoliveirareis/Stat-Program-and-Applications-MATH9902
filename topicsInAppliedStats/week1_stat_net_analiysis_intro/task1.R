data(kapferer)

summary(kapferer, print.adj = F)

heatmap(x = kapferer[, ],      # adjacency matrix of the flobusiness network
        Rowv = NA, Colv = "Rowv", # no dendrograms
        col = c(0, 1),            # black and white
        symm = TRUE,              # indicating if x should be treated symmetrically; 
        # it can only be true when x is a square matrix.
        frame = TRUE)

set.seed(1)
kapferer_num_edges <- dim(kapferer[,])[1]
par(mar = rep(1,4))
plot.network(kapferer, 
             label = seq(1,kapferer_num_edges), 
             edge.col = "grey",
             vertex.cex = 2)


n <- kapferer_num_edges 
p <- 0.08 

g <- rgraph(n = n,            # number of nodes in the network
            m = 1,            # number of networks to generate
            tprob = p,        # fixed tie probability (Bernoulli parameter p)
            mode = 'graph') # directed network (use "graph" for undirected networks)
summary(g)

set.seed(1)
heatmap(g,
        Rowv = NA, Colv = "Rowv", 
        symm = TRUE, 
        col = c(0, 1), 
        frame = TRUE)

set.seed(1)
plot.network(as.network(g, directed = FALSE),
     label = seq(1,n), 
     edge.col = "grey",
     vertex.cex = 2)
