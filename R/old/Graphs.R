#install.packages("igraph")
library(igraph)

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




