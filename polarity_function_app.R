library(qdap)

polarity_text <- function(text){
  polarity(text)$group$ave.polarity
}

