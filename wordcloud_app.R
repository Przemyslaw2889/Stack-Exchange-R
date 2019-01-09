library(wordcloud)

beer <- readRDS("beer_freq_word_comm.rds")
coffee <- readRDS("coffee_freq_word_comm.rds")
gaming <- readRDS("gaming_freq_word_comm.rds")

option_list_wordcloud = list("beer"=beer, "coffee"=coffee, "gaming"=gaming)

wordcloud_app <- function(forum, n = 30){
  data <- option_list_wordcloud[[forum]]
  wordcloud(names(data), data, max.words = n, col = "tan")
  title(paste("Wordcloud of comments:", forum))
}

