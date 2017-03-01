#install.packages("dplyr")
#install.packages("googlesheets")
#install.packages("httr")
#install.packages("igraph")
#install.packages("reshape2")
#install.packages("tidyr")
library(dplyr)
library(googlesheets)
library(httr)
library(igraph)
library(reshape2)
library(tidyr)


## This file takes a table of tweets from Google Sheets and transforms 
## them into a graph format Tableau can use to render the graph visually.


# load tweets
# (make sure to register the googlesheets package with your Google Account 
# first - info on how to do that can be found in the googlesheets vignette: 
# http://htmlpreview.github.io/?https://raw.githubusercontent.com/jennybc/googlesheets/master/vignettes/basic-usage.html)
set_config(config(ssl_verifypeer = 0L))
tweets <- gs_title("politiker tweets") %>% 
  gs_read(ws = "IS_New_Consolidate_Connection", 
          col_names = c("author", "tweet", "url", "date")) %>% 
  mutate(id = row_number()) %>% 
  as.data.frame()
# inspect part of the tweets
#head(tweets, 10)

# extract twitter handles from tweet texts
handles <- tweets %>% 
  group_by(id) %>% 
  mutate(handles = paste(grep("@\\w+", 
                              unlist(strsplit(as.character(tweet), 
                                              " ")), 
                              value = TRUE), 
                         sep = " ", 
                         collapse = " ")) %>% 
  select(id, handles) %>% 
  mutate(handles = strsplit(as.character(handles), " ")) %>% 
  unnest(handles)

# join tweet authors with mentioned handles
mentions <- tweets %>% 
  select(id, author) %>% 
  left_join(handles, by = "id")
  
# generate author-mention matrix
authorMentionMatrix <- table(mentions$author, mentions$handles)

# change it to a Boolean matrix
#authorMentionMatrix[authorMentionMatrix >= 1] <- 1
# transform into a term-term adjacency matrix
authorMatrix <- authorMentionMatrix %*% t(authorMentionMatrix)
# inspect terms numbered 5 to 10
authorMatrix[5:10,5:10]

# build a graph from the above matrix
g <- graph_from_adjacency_matrix(authorMatrix, 
                                 mode = "undirected", 
                                 weighted = TRUE)
# remove loops
g <- simplify(g)
# set labels and degrees of vertices
V(g)$label <- V(g)$name
V(g)$degree <- degree(g)
V(g)$id <- c(1:nrow(authorMatrix))

# create edge list
el <- as_edgelist(g)
el <- data.frame(edgeid = c(1:nrow(el)), 
                 author = V(g)[el[, 1]]$name, 
                 mentioned = V(g)[el[, 2]]$name, 
                 id1 = V(g)[el[, 1]]$id, 
                 id2 = V(g)[el[, 2]]$id, 
                 degree_author = V(g)[el[, 1]]$degree, 
                 weight = E(g)$weight)
el <- el %>% 
  melt(id = c("edgeid", "author", "mentioned", "degree_author", "weight")) %>% 
  arrange(edgeid)

# generate graph layout
# set seed to make the layout reproducible
set.seed(3952)
layout <- layout.circle(g)
plot(g, 
     layout = layout)

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
