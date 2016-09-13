ext_nlp_tags <- function (features){
  features_matrix <- matrix(unlist(features), ncol=2, byrow=TRUE)
  features_df <- as.data.frame(features_matrix, stringsAsFactors = F)
  names(features_df) <- c('pos', 'chunk')
  features_df
}

ext_pattern_index <- function(nlp_tags){
  pos_tags <- paste('', paste(nlp_tags$pos, collapse=' '), '')
  tags_space <- gregexpr(' ', pos_tags)[[1]]
  # MAP EACH POSITION IN THE STRING TO A WORD INDEX
  pos_index <- list()
  for (i in 1:length(tags_space)){
    pos_index[[tags_space[i]]] <- i
  }
  # FIND THE PATTERNS TO EXTRACTION
  extractions <- gregexpr(PATTERN, pos_tags)[[1]]
  # FIND THE WORD INDEXES
  extraction_i <- list()
  j=1
  if(!extractions[1]==-1){
    for(i in  1:length(extractions)){
      tmp <- list(x=pos_index[[extractions[i]-1]], y=pos_index[[extractions[i]-1 + attr(extractions,"match.length")[i]]]-1)
      if (i>1){
        if(extraction_i[[j-1]]$y == (tmp$x-1)){
          extraction_i[[j-1]]$y <- tmp$y
        } else {
          extraction_i[[j]] <- tmp
          j = j+1
        }
      } else {
        extraction_i[[j]] <- tmp
        j = j+1
      }
    }
  }
  extraction_i
}

#EX	Existential there
#WRB	Wh-adverb
#WP	Wh-pronoun
#WP$	Possessive wh-pronoun
#PRP	Personal pronoun
#PRP$	Possessive pronoun
# Added by me
#WDT	Wh-determiner
# Removed by me
#PRP
#pos_tags <- nlp_tags$pos
ext_np_chunks_index <- function(nlp_tags, keepPronouns=T){
  chunk_tags <- paste('', paste(nlp_tags$chunk, collapse=' '), '')
  chunk_space <- gregexpr(' ', chunk_tags)[[1]]
  # MAP EACH POSITION IN THE STRING TO A WORD INDEX
  chunk_index <- list()
  for (i in 1:length(chunk_space)){
    chunk_index[[chunk_space[i]]] <- i
  }
  # FIND THE PATTERNS TO EXTRACTION
  chunks <- gregexpr(CHUNK, chunk_tags)[[1]]
  # FIND THE WORD INDEXES
  chunks_i <- list()
  j=1
  for (i in 1:length(chunks)){
    tmp <- list(x=chunk_index[[chunks[i]-1]], y=chunk_index[[chunks[i]-1 + attr(chunks,"match.length")[i]]]-1)
    
    if(keepPronouns){#tmp$x == tmp$y) {
      if(!nlp_tags$pos[tmp$x] %in% c('EX', 'WRB', 'WP', 'WP$', 'PRP$', 'WDT')){
        chunks_i[[j]] <- tmp
        #print(pos_tags[tmp$x])
        j=j+1
      }
    } else {
      chunks_i[[j]] <- tmp
      j=j+1
    }
  }
  chunks_i
}
#mylist[[length(mylist)+1]] <- obj
extractRelations <- function(nlp_tags) {
  relations <- list()
  extraction_i <- ext_pattern_index(nlp_tags)
  if(length(extraction_i)==0){
    return(relations)
  }
  for (i in 1:length(extraction_i)){
    en1 <- NULL
    en2 <- NULL
    # KEEP REL
    rel <- extraction_i[[i]]
    # DETECTING THE LEFT CHUNK
    chunks_i <- ext_np_chunks_index(nlp_tags)
    for (j in 1:length(chunks_i)){
      # IF THERE IS A NP TO THE LEFT OF REL
      if(chunks_i[[j]]$y < extraction_i[[i]]$x){
        en1 <- chunks_i[[j]]
      }
    }
    # DETECTING THE RIGHT CHUNK
    chunks_i <- ext_np_chunks_index(nlp_tags, F)
    for (j in 1:length(chunks_i)){
      # IF THERE IS A NP TO THE RIGHT OF REL (WITH CROSS LINKS)
      if(chunks_i[[j]]$x > extraction_i[[i]]$x && !chunks_i[[j]]$x > extraction_i[[i]]$y){
        en2 <- chunks_i[[j]]
        en2$x <- extraction_i[[i]]$y+1
        break
      }
      # IF THERE IS A NP TO THE RIGHT OF REL
      if(chunks_i[[j]]$x > extraction_i[[i]]$y){
        en2 <- chunks_i[[j]]
        break
      }
    }
    if(!(is.null(en1) |is.null(en2))) {
      print(c('en1:', s[words][en1$x:en1$y], 'rel:', s[words][rel$x:rel$y], 'en2:', s[words][en2$x:en2$y]))
      
      relations[[length(relations)+1]] <- list(en1=nlp_tags[en1$x:en1$y,], rel=nlp_tags[rel$x:rel$y,], en=nlp_tags[en2$x:en2$y,])
    }
  }
  relations
}