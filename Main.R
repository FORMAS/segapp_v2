library(openNLP)
library(NLP)
require(plyr)


setwd('/Users/ohack/segapp_v2/')
source(file = 'R/NLPTasks.R')
source(file = 'R/Patterns.R')
source(file = 'R/Extractions.R')
source(file = 'R/Util.R')
source(file = 'R/Graphs.R')
source(file = 'R/Features.R')


##################### COMPARING FUNCTIONS GRAPHS
test <- graph.intersection(graph_test, graphs[[100]], byname=F, keep.all.vertices = F)
e_sim <- ldply(1:19, function(i){#length(training_set$y), function(i) {
  comp <- graph.intersection(graph_test, graphs[i], keep.all.vertices = F)
  print(c(ecount(comp), vcount(comp), ecount(training_set$graphs[[i]]), vcount(training_set$graphs[[i]]), ecount(graph_test), vcount(graph_test)))
})
################## CALCULATING FEATURES

f1(nlp_tags[[49]])
f2(nlp_tags[[49]])
#FOR F12

tags_f12 <- lapply(df_training, function(l){
  tag(l['sentence'])
})
f12(tags_f12[[1]][tags_f12[[1]]$type=='word'])

#################################################
# EXAMPLE SENTENCE
s <- as.String("The final decision about UFM has still space for mayor George")
# PIPELINE EXTRACTIONS
tags <- tag(s)
# SUBSET WORDS
words <- tags[tags$type=='word']
# EXTRACT FEATURES [POS, CHUNK]
nlp_tags <- ext_nlp_tags(words$features)
# CORRECTING OPENNLP ERROS
nlp_tags[nlp_tags$pos=='POS',]$chunk <- 'I-NP'
nlp_tags[nlp_tags$pos%in%c(',', '.'),]$chunk <- 'O'
# ADDING WORDS TO THE VECTOR
nlp_tags$words <- s[words]
# EXTRACT RELATIONS
relations <- extractRelations(nlp_tags)
# CREATE A GRAPH
testGraph <- createGraph(relations[[1]]$rel$pos)
plot(testGraph)

#printByIndex(s, words, extraction_i)

