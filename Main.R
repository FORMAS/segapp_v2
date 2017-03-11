# PACKAGES INSTALLATION
install.packages('openNLP')
install.packages('NLP')
install.packages('plyr')
install.packages('aod')
install.packages('XML')
# MODEL INSTALLATION
install.packages("openNLPmodels.en", repos = "http://datacube.wu.ac.at/", type = "source")
# LIBRARY LOADING
library(openNLP)
library(NLP)
library(plyr)
library(aod)
library(XML)
# SOURCING FILES
source(file = 'R/NLPTasks.R')
source(file = 'R/Patterns.R')
source(file = 'R/Extractions.R')
source(file = 'R/Util.R')
source(file = 'R/ReadDB.R')
source(file = 'R/Features.R')
#################################################
# EXAMPLE SENTENCE
#s <- as.String("The final decision about UFM has still space for mayor George")
s <- as.String("He died en route to hospital.")
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
# CONVERT A RELATION TO A EXTRACTION
df_test <- list(c(en1=paste(relations[[1]]$en1$words, collapse = ' '),
                  rel=paste(relations[[1]]$rel$words, collapse = ' '),
                  en2=paste(relations[[1]]$en2$words, collapse = ' '),
                  sentence=s))
# EXTRACT FEATURES
tmp <- list(
  f1=f1(df_test), 
  f2=f_prep(list(nlp_tags), 'for'),
  f3=f_prep(list(nlp_tags), 'on'),
  f4=f_prep(list(nlp_tags), 'of'),
  f5=f_prep(list(nlp_tags), 'to'),
  f6=f_prep(list(nlp_tags), 'in'),
  f9=f9(df_test),
  f10=f10(df_test),
  f11=f11(df_test),
  f12=f12(list(words))
)
# TRAIN A CLASSIFIER
cls <- glm(y ~ f1 + f2 + f3 + f4 + f5 + f6 + f9 + f10 + f11 + f12, data = features, family = "binomial")
summary(cls)
# Confidence Intervals
confint(cls)
# PREDICT THE NEW EXTRACTION
predict(cls, newdata = tmp, type = "response")
