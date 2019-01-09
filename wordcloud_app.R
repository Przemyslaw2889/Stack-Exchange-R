library(wordcloud)
# library(tm)
# library(stringi)
# library(textstem)
# 
# clean_text <- function(text){
#  text <- lemmatize_strings(text)
#  text <- removePunctuation(text)
#  text <- tolower(text)
#  text <- removeWords(text,c(stopwords("en"),"coffee","beer"))
#  text <- stripWhitespace(text)
#  text
# }
# 
# #COMMENTS
# gaming_comments <- readRDS("gaming_comment_clean.rd")
# gaming_comments <- gaming_comments[sample(1:length(gaming_comments),size = 3200,replace = FALSE)]
# 
# coffee_comments <- clean_text(read.csv("data/coffee.stackexchange.com/Comments.csv")$Text)
# beer_comments <- clean_text(read.csv("data/beer.stackexchange.com/Comments.csv")$Text)
# 
# #corpus
# coffee_comments_corp <- VCorpus(VectorSource(coffee_comments))
# coffee_tdm_comm <- as.matrix(TermDocumentMatrix(coffee_comments_corp))
# coffee_freq_word <- rowSums(coffee_tdm_comm)
# coffee_freq_word <- sort(coffee_freq_word, decreasing = TRUE)
# 
# 
# gaming_comments_corp <- VCorpus(VectorSource(gaming_comments))
# gaming_tdm_comm <- as.matrix(TermDocumentMatrix(gaming_comments_corp))
# gaming_freq_word <- rowSums(gaming_tdm_comm)
# gaming_freq_word <- sort(gaming_freq_word, decreasing = TRUE)
# 
# 
# beer_comments_corp <- VCorpus(VectorSource(beer_comments))
# beer_tdm_comm <- as.matrix(TermDocumentMatrix(beer_comments_corp))
# beer_freq_word <- rowSums(beer_tdm_comm)
# beer_freq_word <- sort(beer_freq_word, decreasing = TRUE)
# 
# saveRDS(coffee_freq_word, "coffee_freq_word_comm.rds")
# saveRDS(beer_freq_word, "beer_freq_word_comm.rds")
# saveRDS(gaming_freq_word, "gaming_freq_word_comm.rds")


beer <- readRDS("beer_freq_word_comm.rds")
coffee <- readRDS("coffee_freq_word_comm.rds")
gaming <- readRDS("gaming_freq_word_comm.rds")

wordcloud_app <- function(forum, n = 30){
  wordcloud(names(forum),forum ,max.words = n, col = "tan")
  title(paste("Wordcloud komentarzy dla forum", deparse(substitute(forum))))
  }

