f1 <- function(df_training){
  length(df_training$en1[1])
  if(nchar(df_training$sentence[1]) - nchar(df_training$en1[1])+nchar(df_training$rel[1])+nchar(df_training$en2[1]) < 30){
    return(1)
  }
  return(0)
}

f4 <- function(nlp_tags){
  last_prepo <- NULL
  for(i in 1:nrow(nlp_tags)){
    if (nlp_tags$pos[i] %in% c('IN', 'TO')) {
      last_prepo <- nlp_tags$words[i] 
    }
  }
  if(!is.null(last_prepo)){
    if(tolower(last_prepo)=='in')
      return(1)
  }
  0
}

f5 <- function(nlp_tags){
  last_prepo <- NULL
  for(i in 1:nrow(nlp_tags)){
    if (nlp_tags$pos[i] %in% c('IN', 'TO')) {
      last_prepo <- nlp_tags$words[i] 
    }
  }
  if(!is.null(last_prepo)){
    if(tolower(last_prepo)=='for')
      return(1)
  }
  return(0)
}

f6 <- function(nlp_tags){
  last_prepo <- NULL
  for(i in 1:nrow(nlp_tags)){
    if (nlp_tags$pos[i] %in% c('IN', 'TO')) {
      last_prepo <- nlp_tags$words[i] 
    }
  }
  if(!is.null(last_prepo)){
    if(tolower(last_prepo)=='on')
      return(1)
  }
  0
}

f7 <- function(nlp_tags){
  last_prepo <- NULL
  for(i in 1:nrow(nlp_tags)){
    if (nlp_tags$pos[i] %in% c('IN', 'TO')) {
      last_prepo <- nlp_tags$words[i] 
    }
  }
  if(!is.null(last_prepo)){
    if(tolower(last_prepo)=='of')
      return(1)
  }
  0
}


f8 <- function(nlp_tags){
  last_prepo <- NULL
  for(i in 1:nrow(nlp_tags)){
    if (nlp_tags$pos[i] %in% c('IN', 'TO')) {
      last_prepo <- nlp_tags$words[i] 
    }
  }
  if(!is.null(last_prepo)){
    if(tolower(last_prepo)=='to')
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

f10 <- function(sentence, rel, en2){
  rel_en2 <- paste(rel, en2, collapse = ' ', sep=' ')
  if(gregexpr(rel_en2, sentence)[[1]][[1]]==-1){
    return(0)
  }
  return(1)
}

f11 <- function(df_training){
  ?mapply
  mapply(function(l){
    return(1)
    #en1_rel_en2 <- paste(l[2], l[3], l[4], collapse = ' ', sep=' ')
    
    #if(gregexpr(en1_rel_en2, l[5])==-1){
    #  return(1)
    #}
    #return(0)
  }, df_training[1:20,])
  
  ?apply
  tmp <- apply(X = df_training[2:2,], 1,FUN = function(l){ 
    en1_rel_en2 <- paste(l[2], l[3], l[4], collapse = ' ', sep=' ')
    
    #if(gregexpr(en1_rel_en2, l[5])==-1){
    #  return(1)
    #}
    #return(0)
  })
  
  en1_rel_en2 <- paste(en1, rel, en2, collapse = ' ', sep=' ')
  if([[1]][[1]]==-1){
    return(0)
  }
  return(1)
}

f12 <- function(tags_f12){
  sapply(tags_f12, function(l){ ifelse(length(l)>34, 1, 0) })
}