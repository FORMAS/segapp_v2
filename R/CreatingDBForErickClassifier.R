library(openNLP)
library(NLP)
library(plyr)

setwd('/Users/ohack/segapp_v2/')
source(file = 'R/NLPTasks.R')
source(file = 'R/Patterns.R')
source(file = 'R/Extractions.R')
source(file = 'R/Util.R')
source(file = 'R/Graphs.R')
source(file = 'R/Features.R')

#df_training <- readTrainingDF("data/nyt-extractions-all-labeled.txt", 2000)
df_training <- readTrainingDF("data/wiki-extractions-all-labeled.txt", 1900)
head(df_training)

tags_rel <- lapply(df_training, function(l){
  tag(l['rel'])
})

tags_sentence <- lapply(df_training, function(l){
  tag(l['sentence'])
})

tags_en1 <- lapply(df_training, function(l){
  tag(l['en1'])
})

tags_en2 <- lapply(df_training, function(l){
  tag(l['en2'])
})


nlp_tags <- sapply(1:length(df_training), function(i){
  tags_rel_ <- tags_rel[[i]][tags_rel[[i]]$type=='word']
  rel <- paste(as.String(df_training[[i]][['rel']])[tags_rel_], collapse = ' ')
  pos_rel <- paste(ext_nlp_tags(tags_rel_$features)$pos, collapse = ' ')
  
  tags_sentence_ <- tags_sentence[[i]][tags_sentence[[i]]$type=='word']
  sentence <- paste(as.String(df_training[[i]][['sentence']])[tags_sentence_], collapse = ' ')
  pos_sentence <- paste(ext_nlp_tags(tags_sentence_$features)$pos, collapse = ' ')
  
  tags_en1_ <- tags_en1[[i]][tags_en1[[i]]$type=='word']
  en1 <- paste(as.String(df_training[[i]][['en1']])[tags_en1_], collapse = ' ')
  pos_en1 <- paste(ext_nlp_tags(tags_en1_$features)$pos, collapse = ' ')
  
  tags_en2_ <- tags_en2[[i]][tags_en2[[i]]$type=='word']
  en2 <- paste(as.String(df_training[[i]][['en2']])[tags_en2_], collapse = ' ')
  pos_en2 <- paste(ext_nlp_tags(tags_en2_$features)$pos, collapse = ' ')
  
  list(en1=en1, pos_en1=pos_en1, rel=rel, pos_rel=pos_rel, en2=en2, pos_en2=pos_en2, sentence=sentence, pos_sentence=pos_sentence, y=as.numeric(df_training[[i]]['y']) )
})

# 100
nlp_tags <- t(as.data.frame(nlp_tags))

tmp <- nlp_tags[sample(nrow(nlp_tags), 100),]
write.table(tmp, sep = '#', row.names = F, file = 'output/nyt_tags_sample_100.csv')
# 200
tmp <- nlp_tags[sample(nrow(nlp_tags), 200),]
write.table(nlp_tags, sep = '#', row.names = F, file = 'output/nyt_tags_sample_200.csv')
# 300
tmp <- nlp_tags[sample(nrow(nlp_tags), 300),]
write.table(nlp_tags, sep = '#', row.names = F, file = 'output/nyt_tags_sample_300.csv')
