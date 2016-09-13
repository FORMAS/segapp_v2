#install.packages("igraph")
library(igraph)
#g <- graph.empty()
#g <- g + vertex(nos)
#g <- g + edge('DT', 'JJ')

createGraph <- function(pos_list) {
  g <- graph.empty()
  g <- g + vertex(pos_list)
  l_tag <- NULL
  for(i in pos_list){
    if(is.null(l_tag)){
      l_tag <- i
    } else {
      g <- g + edge(l_tag, i)
      l_tag <- i
    }
  }
  g
}

#training <- read.table(file = 'data/nyt-extractions-all-labeled.txt', sep = '\t', fill = T)
#names(training) <- c('id', 'en1', 'rel', 'en2')

#length(training)

# nao funciona porque as threads não compartilham informação
readTrainingDF <- function(path, n_lines){
  no_lines = n_lines
  #no_lines = 2200
  correct_length = 5
  file_con = file(path, "r")
  file_con = file("data/nyt-extractions-all-labeled.txt", "r")
  result <- list()
  j <- 1
  for(i in 1:no_lines){
    dum = strsplit(readLines(file_con, n = 1), split = "\t")[[1]]
    if(length(dum) == 1) { 
      sentence <- dum
      #cat(sprintf("Skipped line %s\n", line))
    }
    if(length(dum) == correct_length) {
      dum[6] <- sentence
      dum <- gsub('"', '', dum)
      names(dum) <- c('id', 'en1', 'rel', 'en2', 'y', 'sentence')
      result[[j]] <- dum
      j<-j+1
    }
  }
  result
}

#plot(graph_test)


# create an example graph
#D <- read.table(header=T,text=
#                  'from to
#                A B
#                A C
#                C D
#                C F
#                C E
#                D E
#                D F
#                E F')
#
#g1 <- graph.data.frame(D,directed=F)
# plot the original graph
#plot(g1)
# find all the largest cliques (returns a list of vector of vertiex ids)
#a <- largest.cliques(g1)
# let's just take the first of the largest cliques
# (in this case there's just one clique)
#clique1 <- a[[1]]
# subset the original graph by passing the clique vertices
#g2 <- induced.subgraph(graph=g1,vids=clique1)
# plot the clique
#plot(g2)
