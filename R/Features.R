f1 <- function(nlp_tags){
  last_prepo <- NULL
  for(i in 1:nrow(nlp_tags)){
    if (nlp_tags$pos[i] %in% c('IN', 'TO')) {
      last_prepo <- nlp_tags$words[i] 
    }
  }
  if(!is.null(last_prepo)){
    if(last_prepo%in%c('to', 'To', 'TO'))
      return(1)
  }
  0
}


f9 <- function(sentence, en1, rel){
  en1_rel <- paste(en1, rel, collapse = ' ', sep=' ')
  class(en1_rel)
  if(gregexpr(en1_rel, sentence)[[1]][[1]]==-1){
    return(0)
  }
  return(1)
}
