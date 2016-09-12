printByIndex <- function(sentence, tags_model, indexes){
  for (i in indexes){
    print(sentence[tags_model][i$x:i$y])
  }
}