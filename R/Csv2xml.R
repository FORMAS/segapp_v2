ts <- read.csv2('output/nyt_tags.csv', sep='%', header = T)
# CONCATENAR OS COMPONENTES DA EXTRACAO
ts$seg <- paste(ts$pos_en1, ts$pos_rel, ts$pos_en2)
# TABELA DE FREQUENCIA
freq_table <- as.data.frame(table(ts))
# EXTRAIR SOMENTE OS QUE TIVERAM FREQ
freq_table <- freq_table[freq_table$Freq>0,]
# FINDING ONLY THE DUPLICATED ROWS
freq_dup <- freq_table[duplicated(freq_table$seg),]
freq_nao_dup <- freq_table[duplicated(freq_table$seg, fromLast = T),]
# SUM IT
sum(freq_dup$Freq) + sum(freq_nao_dup$Freq)
#####################################
# SAMPLING IT
ts <- ts[sample(nrow(ts), 100),]

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
write.table(x = saveXML(xml), file = 'output/sample_nyt_tags.xml', row.names = F, col.names = F)

writeLines(text = saveXML(xml), con = 'output/sample_nyt_tags.xml')

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
