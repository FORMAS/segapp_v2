f1 <- function(sent_tags){
  sapply(sent_tags, function(l){
    #l['y']
    if((nchar(l['sentence']) - (nchar(l['en1'])+nchar(l['rel'])+nchar(l['en2']))) < 30){
      #print(nchar(l['sentence']) - (nchar(l['en1'])+nchar(l['rel'])+nchar(l['en2'])) )
      return(1)
    }
    return(0)
  })
}

f_prep <- function(sent_tags, prep){
  sapply(sent_tags, function(l){
    # Convert all words to characters (the one's that had only one word were character and the others were String)
    l$words <- as.character(l$words)
    l <- l[l[['pos']] %in% c('IN', 'TO'), ]
    if(nrow(l) > 0 && tolower(l$words[nrow(l)])==prep){
      return(1)
    }
    return(0)
  })
}

f9 <- function(sent_tags){
  sapply(sent_tags, function(l){
    en1_rel <- paste(l['en1'], l['rel'], collapse = ' ', sep=' ')
    if(gregexpr(en1_rel, l['sentence'])[[1]]!=-1){
      return(1)
    }
    return(0)
  })
}

f10 <- function(sent_tags){
  sapply(sent_tags, function(l){
    rel_en2 <- paste(l['rel'], l['en2'], collapse = ' ', sep=' ')
    if(gregexpr(rel_en2, l['sentence'])[[1]]!=-1){
      return(1)
    }
    return(0)
  })
}

f11 <- function(sent_tags){
  sapply(sent_tags, function(l){
    en1_rel_en2 <- paste(l['en1'], l['rel'], l['en2'], collapse = ' ', sep=' ')
    if(gregexpr(en1_rel_en2, l['sentence'])[[1]]!=-1){
      return(1)
    }
    return(0)
  })
}


f12 <- function(sent_tags){
  sapply(sent_tags, function(l){
    if(length(l[l$type=='word']) < 30){
      return(1)
    }
    return(0)
  })
}