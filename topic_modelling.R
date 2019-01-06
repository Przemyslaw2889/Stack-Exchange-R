library(topicmodels)
library(tidytext)
library(RTextTools)
library(dplyr)
library(stringi)
library(textstem)
library(tm)

clean_html <- function(texts){
  print("Cleaning HTML")
  texts <- stri_replace_all_regex(texts, "<.*?>", "")
  texts <- stri_replace_all_fixed(texts, "\n", "")
  texts <- stri_replace_all_fixed(texts, "\t", "")
  texts <- stri_replace_all_fixed(texts, "\\", "")
  texts
}

create_clean_matrix <- function(text){
  text <- clean_html(text);  print("Cleaning HTML: done")
  text <- lemmatize_strings(text); print("Lemmatization: done ")
  text <- removePunctuation(text); print("Removing punctuation: done")
  text <- tolower(text)
  text <- removeWords(text,stopwords("en"))
  text <- stripWhitespace(text)
  
  matrix <- create_matrix(text, language="english", 
                          removeStopwords=TRUE, removeNumbers=TRUE, 
                          stemWords = FALSE, removePunctuation = TRUE,
                          toLower = TRUE,stripWhitespace = TRUE)
  print("Matrix creation: done")
  # Getting rid of unused elements
  ui <- unique(matrix$i)
  matrix <- matrix[ui,]
  matrix
}

Posts_coffee <- read.csv("data/coffee.stackexchange.com/Posts.csv")
Posts_gaming <- read.csv("data/gaming.stackexchange.com/Posts.csv")
texts <- Posts_coffee$Body
text_matrix <- create_clean_matrix(texts)

n_topics <- 2
topic_lda <- LDA(text_matrix, k=n_topics, control=list(seed=42))
summary(topic_lda)
topic_lda
topics <- tidy(topic_lda, matrix = "beta")
topics %>% group_by(topic) %>% top_n(n=15, wt=beta) %>% arrange(topic, desc(beta))

topics %>% filter(topic==1) %>% arrange(desc(beta))
topics %>% filter(topic==2) %>% arrange(desc(beta))


