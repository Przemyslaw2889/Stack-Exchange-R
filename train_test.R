#dane do pobrania tutaj: ai.stanford.edu/~amaas/data/sentiment/aclImdb_v1.tar.gz
source("df_from_xml.R")
options(stringsAsFactors = FALSE)
Posts <- xml_data_frame("coffee.stackexchange.com/Posts.xml")
#biblioteki
library(qdap)
library(tm)
library(RTextTools)
library(e1071)
library(textstem)
library(randomForest)
#stwrzenie zbioru testoweg i treningowego
pliki_pos <- paste("data/train/pos/",list.files("data/train/pos/"),sep="")
pliki_neg <- paste("data/train/neg/",list.files("data/train/neg/"),sep="")
n <- 1500
text <- numeric(2*n)
for (i in 1:n){
  text[i] <- readLines(pliki_pos[i])
  text[i+n] <- readLines(pliki_neg[i])
}


train <- data.frame(text = text, klasa = c(rep("positive",n),rep("negative",n)))

n <- 500
test_pos <- paste("data/test/pos/",list.files("data/test/pos/"),sep = "")
test_neg <- paste("data/test/neg/",list.files("data/test/neg/"),sep="")
text_test <- numeric(2*n)
for (i in 1:n){
  text_test[i] <- readLines(test_pos[i])
  text_test[i+n] <- readLines(test_neg[i])
  }

test <- data.frame(text = text_test,klasa = c(rep("positive",n),rep("negative",n)))

#obrobka danych tekstowych, lematyzacja, zmiana na male liery, usuniecie znakw interpunkcyjnych itp
data_train_test_post <- data.frame(text = c(train$text,test$text,Posts$Body),
                                   klasa = c(train$klasa,test$klasa,rep("post",nrow(Posts))),
                                   typ = c(rep("train",nrow(train)),rep("test",nrow(test)),rep("post",nrow(Posts))))

data_train_test_post$text <- lemmatize_strings(data_train_test_post$text)
data_train_test_post$text <- removePunctuation(data_train_test_post$text)
data_train_test_post$text <- tolower(data_train_test_post$text)
data_train_test_post$text <- removeWords(data_train_test_post$text,stopwords("en"))
data_train_test_post$text <- stripWhitespace(data_train_test_post$text)

data_train_test_post_m <- create_matrix(data_train_test_post$text, language="english", 
                      removeStopwords=TRUE, removeNumbers=TRUE, 
                      stemWords = FALSE,removePunctuation = TRUE,toLower = TRUE,stripWhitespace = TRUE) 

data_train_test_post_m <- as.matrix(data_train_test_post_m)


#budowa modelu(random forest), model dopasowany tylko na czesci danych ze wzgledu na czas
classifier <- randomForest(data_train_test_post_m[1400:1600,], as.factor(data_train_test_post$klasa[1400:1600]) )

predicted <- predict(classifier, data_train_test_post_m[3000:4000,])

table(data_train_test_post[3000:4000,"klasa"], predicted)
recall_accuracy(data_train_test_post[3000:4000,"klasa"], predicted)


#predykcja postow
pred_post <- unname(predict(classifier,newdata = data_train_test_post_m[4000:nrow(data_train_test_post_m),]))
table(pred_post)

