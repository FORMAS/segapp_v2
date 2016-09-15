install.packages('XML')
library(XML)

ts<-training_set_erick_alg[1:10,]

xml <- xmlTree()
xml$addTag("exemplos", close=FALSE)
for (i in 2:nrow(ts)) {
  xml$addTag("sentenca", close=FALSE)
  xml$addTag('frase', ts$sentence)
  xml$addTag('entidade1', ts$en1[i])
  xml$addTag('entidade2', ts$en2[i])
  xml$addTag('relacao', ts$rel[i])
  xml$addTag('valida', '1')#ts$y[i])
  words <- c(strsplit(as.character(ts$en1[i]), " ")[[1]], strsplit(as.character(ts$rel[i]), " ")[[1]], strsplit(as.character(ts$en2[i]), " ")[[1]])
  pos <- c(strsplit(as.character(ts$pos_en1[i]), " ")[[1]], strsplit(as.character(ts$pos_rel[i]), " ")[[1]], strsplit(as.character(ts$pos_en2[i]), " ")[[1]])
  for (j in 1:length(words)) {
    xml$addTag('palavra', words[j])
    xml$addTag('etiqueta', pos[j])
  }
  xml$closeTag()
}
xml$closeTag()
cat(saveXML(xml))

