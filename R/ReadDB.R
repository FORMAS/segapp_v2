readTrainingDF <- function(path, n_lines){
  no_lines = n_lines
  #no_lines = 2200
  correct_length = 5
  file_con = file(path, "r")
  #file_con = file("data/nyt-extractions-all-labeled.txt", "r")
  result <- list()
  j <- 1
  for(i in 1:no_lines){
    dum = strsplit(readLines(file_con, n = 1), split = "\t")[[1]]
    if(length(dum) == 1) { 
      sentence <- dum
      #cat(sprintf("Skipped line %s\n", line))
    }
    if(length(dum) == correct_length) {
      dum[6] <- sentence
      dum <- gsub('"', '', dum)
      names(dum) <- c('id', 'en1', 'rel', 'en2', 'y', 'sentence')
      result[[j]] <- dum
      j<-j+1
    }
  }
  close(file_con)
  result
}