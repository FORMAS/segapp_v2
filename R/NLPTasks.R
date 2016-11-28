library('openNLP')

sent_token_ann <- Maxent_Sent_Token_Annotator()
word_token_ann <- Maxent_Word_Token_Annotator()
pos_tag_ann <- Maxent_POS_Tag_Annotator()
chunk_ann <- Maxent_Chunk_Annotator()

tag <- function(s){
  ann <- list(sent_token_ann, word_token_ann, pos_tag_ann, chunk_ann)
  #ann <- list(sent_token_ann, word_token_ann)
  tags <- annotate(s, ann)
  #tags <- lapply(X = s, FUN = annotate, f=ann)
  tags
}