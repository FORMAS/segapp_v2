library(openNLP)
library(NLP)
require(plyr)

sent_token_ann <- Maxent_Sent_Token_Annotator()
word_token_ann <- Maxent_Word_Token_Annotator()
pos_tag_ann <- Maxent_POS_Tag_Annotator()
chunk_ann <- Maxent_Chunk_Annotator()


setwd('/Users/ohack/segapp_v2/')
source(file = 'R/NLPTasks.R')
source(file = 'R/Patterns.R')
source(file = 'R/Extractions.R')
source(file = 'R/Util.R')
source(file = 'R/Graphs.R')
source(file = 'R/Features.R')


df_training[[1]]['y']
###################### TRAINING SET GRAPHS
############### READING IT
df_training <- readTrainingDF("data/nyt-extractions-all-labeled.txt", 2292)
#df_training$rel <- lapply(X = df_training$rel, as.String)
tags <- lapply(df_training, function(l){
  tag(l['rel'])
})
head(tags)
########## CONSTRUCT NLP VECTOR
nlp_tags <- list()
for (i in 1:length(df_training)){
  tags_word <- tags[[i]][tags[[i]]$type=='word']
  nlp_tags[[i]] <- ext_nlp_tags(tags_word$features)
  rel <- as.String(df_training[[i]]['rel'])
  nlp_tags[[i]]$words <- rel[tags_word]
  #nlp_tags[[i]]$sentence <- df_training$sentence[1]
  #nlp_tags[[i]]$y <- df_training$y[i]
}
nlp_tags[[2]]
######################### CONSTRUCTING GRAPHS
graphs <- lapply(nlp_tags, function(l){
  createGraph(l$pos)
})
#4-10
plot(graphs[[15]])
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
tags_f12 <- tag(df_training$sentence)
f12(tags_f12)

#################################################
# EXAMPLE SENTENCE
s <- as.String("The committee is planning to hear from him this month.")
# PIPELINE EXTRACTIONS
tags <- tag(s)
# SUBSET WORDS
words <- tags[[1]][tags[[1]]$type=='word']
# EXTRACT FEATURES [POS, CHUNK]
nlp_tags <- ext_nlp_tags(words$features)
# CORRECTING OPENNLP ERROS
nlp_tags[nlp_tags$pos=='POS',]$chunk <- 'I-NP'
nlp_tags[nlp_tags$pos%in%c(',', '.'),]$chunk <- 'O'
# ADDING WORDS TO THE VECTOR
nlp_tags$words <- s[words]

relations <- extractRelations(nlp_tags)
graph_test <- createGraph(relations[[1]]$rel$pos)

#printByIndex(s, words, extraction_i)

