options(stringsAsFactors = FALSE)

library(qdap)
library(tm)
library(RTextTools)
library(e1071)
library(textstem)
#library(randomForest)
library(stringi)
#library(caret)
#library(dplyr)
library(class)
#library(reshape2)


new_data <- function(x, names){
  x <- lemmatize_strings(x)
  x <- removePunctuation(x)
  x <- tolower(x)
  x <- removeWords(x,stopwords("en")) 
  x <- stripWhitespace(x)
  data <- matrix(data = 0,nrow = length(x),ncol = length(names))
  for (i in 1:length(x)){
    data[i,] <- stri_count_fixed(x[i],names,opts_fixed = NULL)
  }
  colnames(data) <- names
  data
}


data_train_m <- readRDS("data_train_m.rds")
rf_high_var <- readRDS("rf_high_var.rds")
var <- apply(data_train_m, 2, var)
data_train_m <- data_train_m[,var > 0.001]
words <- colnames(data_train_m)

sentyment <- function(text, word = words){
  row <- new_data(text,word)
  predict(object = rf_high_var, row)
}

