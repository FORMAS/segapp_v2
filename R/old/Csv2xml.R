ts <- read.csv2('output/nyt_tags.csv', sep='%', header = T)
# SAMPLING
ts <- ts[sample(nrow(ts), 200),]

xml <- xmlTree()
xml$addTag("exemplos", close=FALSE)
for (i in 2:nrow(ts)) {
  xml$addTag("sentenca", close=FALSE)
  xml$addTag('frase', ts$sentence[i])
  xml$addTag('entidade1', ts$en1[i])
  xml$addTag('entidade2', ts$en2[i])
  xml$addTag('relacao', ts$rel[i])
  xml$addTag('valida', ts$y[i])
  print(ts$y[i])
  words <- c(strsplit(as.character(ts$en1[i]), " ")[[1]], strsplit(as.character(ts$rel[i]), " ")[[1]], strsplit(as.character(ts$en2[i]), " ")[[1]])
  pos <- c(strsplit(as.character(ts$pos_en1[i]), " ")[[1]], strsplit(as.character(ts$pos_rel[i]), " ")[[1]], strsplit(as.character(ts$pos_en2[i]), " ")[[1]])
  for (j in 1:length(words)) {
    xml$addTag('palavra', words[j])
    xml$addTag('rotulo', pos[j])
  }
  xml$closeTag()
}
xml$closeTag()
xml_backup <- saveXML(xml)

xm_b_t <- strsplit(xml_backup, '\n')
write.table(x = saveXML(xml), file = 'output/sample_nyt_tags_200.xml', row.names = F, col.names = F)

writeLines(text = saveXML(xml), con = 'output/sample_nyt_tags_200.xml')

############## Results for the erick method
install.packages('ROCR')
library (ROCR)
result_seg <- factor(as.numeric(strsplit('0 0 0 0 0 0 0 1 1 0 0 0 0 0 1 0 0 1 1 1 0 0 0 0 0 0 0 0 1 1 1 1 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 0 1 0 0 1 0 0 1 0 0 0 0 0 0 1 1 1 1 0 0 0 0 1 0 0 0 0 0 0 0 1 1 0', split = ' ')[[1]]))
length(result_seg)
ts <- read.csv2('output/wiki_tags.csv', sep='%', header = T)
ts <- ts[1:length(result_seg),]
ts$y <- factor(ts$y)

pred <- prediction(result_seg, ts$y);

library(caret)

precision <- posPredValue(result_seg, ts$y)
recall <- sensitivity(result_seg, ts$y)

F1 <- (2 * precision * recall) / (precision + recall)
