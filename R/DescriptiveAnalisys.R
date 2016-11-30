df <- read.csv2('output/nyt+wiki_tags.csv', sep='%', header = T)
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