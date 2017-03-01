#install.packages("dplyr")
#install.packages("igraph")
#install.packages("reshape2")
library(dplyr)
library(igraph)
library(reshape2)


## This file takes a term-document matrix and transforms it into a graph
## format Tableau can use to render the graph visually.


# load termDocMatrix (originally from: http://www.rdatamining.com/data)
load("termDocMatrix.rdata")
# inspect part of the matrix
#termDocMatrix[, 1:20]

# change it to a Boolean matrix
termDocMatrix[termDocMatrix >= 1] <- 1
# transform into a term-term adjacency matrix
termMatrix <- termDocMatrix %*% t(termDocMatrix)
# inspect terms numbered 5 to 10
#termMatrix[5:10,5:10]

# build a graph from the above matrix
g <- graph_from_adjacency_matrix(termMatrix, 
                                 mode = "undirected", 
                                 weighted = TRUE)
# remove loops
g <- simplify(g)
# set labels and degrees of vertices
V(g)$label <- V(g)$name
V(g)$degree <- degree(g)
V(g)$id <- c(1:nrow(termMatrix))

# create edge list
el <- as_edgelist(g)
el <- data.frame(edgeid = c(1:nrow(el)), 
                 topic1 = V(g)[el[, 1]]$name, 
                 topic2 = V(g)[el[, 2]]$name, 
                 id1 = V(g)[el[, 1]]$id, 
                 id2 = V(g)[el[, 2]]$id, 
                 degree1 = V(g)[el[, 1]]$degree, 
                 weight = E(g)$weight)
el <- el %>% 
  melt(id = c("edgeid", "topic1", "topic2", "degree1", "weight")) %>% 
  arrange(edgeid)

# generate graph layout
# set seed to make the layout reproducible
set.seed(3952)
layout <- layout.fruchterman.reingold(g)
#plot(g, 
#     layout = layout)

# extract vertex coordinates
vc <- data.frame(id = c(1:nrow(layout)), 
                 x = layout[, 1], 
                 y = layout[, 2])

# complete graph format
graph1 <- el %>% 
  filter(variable == "id1") %>% 
  left_join(vc, 
            by = c("value" = "id"))
graph2 <- el %>% 
  filter(variable == "id2") %>% 
  left_join(vc, 
            by = c("value" = "id"))
graph <- rbind(graph1, graph2) %>% 
  mutate(order = substr(variable, 3, 3)) %>% 
  select(-variable) %>% 
  rename(vertexid = value) %>% 
  arrange(edgeid, order)

# export graph to disk
write.csv(graph, 
          file = "graph.csv")