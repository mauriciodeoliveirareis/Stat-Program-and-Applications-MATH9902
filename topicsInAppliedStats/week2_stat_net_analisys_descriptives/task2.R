#Load the kapferer network dataset included in statnet.
library(statnet)
data(kapferer)
summary(kapferer, print.adj = F)

#Calculate the density of the network.
#y <- kapferer # undirected network
n <- network.size(kapferer)
set.seed(1)
plot.network(kapferer,
             label = 1:n,
             vertex.cex = 2)

summary(kapferer ~ density)


#Calculate and plot the degree distribution of the network. Identify the node(s) with minimum and maximum degree.
d_i <- degree(y, 
              gmode = 'graph') # undirected network

barplot(table(d_i) / n, 
        main = "Degree distribution",
        col = "skyblue",
        xlab = "Degree",
        ylab = "Relative frequency")

#find the nodes with minimum degree
which(d_i == min(d_i))

#find the nodes with maximum degree
which(d_i == max(d_i))

#Calculate the betweenness centrality and identify the most important node(s).
set.seed(2)
par(mar = rep(0, 4))
gplot.target(y[, ], 
             betweenness(y), 
             gmode = "graph", 
             label = 1:39, 
             vertex.cex = 0.3,
             edge.col = 'lightgrey',
             label.cex = 1,
             circ.lab.cex = 0.5) #node 16 is the center 


#Calculate the clustering coefficient of the network.
gtrans(y[, ],           
       mode = "graph") 