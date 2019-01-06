#dane do pobrania tutaj: ai.stanford.edu/~amaas/data/sentiment/aclImdb_v1.tar.gz
#source("df_from_xml.R")
options(stringsAsFactors = FALSE)
Posts_coffee <- read.csv("data/coffee.stackexchange.com/Posts.csv")
#biblioteki
library(qdap)
library(tm)
library(RTextTools)
library(e1071)
library(textstem)
library(randomForest)
library(stringi)
library(caret)
library(dplyr)
library(class)
library(reshape2)
#stworzenie zbioru testoweg i treningowego
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


#obrobka danych tekstowych, lematyzacja, zmiana na male liery, usuniecie znakow interpunkcyjnych itp
data_train_test_post <- data.frame(text = c(train$text,test$text,Posts_coffee$Body),
                                   klasa = c(train$klasa,test$klasa,rep("post_coffee",nrow(Posts_coffee))),
                                   typ = c(rep("train",nrow(train)),rep("test",nrow(test)),rep("post_coffee",nrow(Posts_coffee))))

clean_text <- function(text){
  text <- lemmatize_strings(text)
  text <- removePunctuation(text)
  text <- tolower(text)
  text <- removeWords(text,stopwords("en"))
  text <- stripWhitespace(text)
  text
}

data_train_test_post$text <- clean_text(data_train_test_post$text)

data_train_test_post_m <- create_matrix(data_train_test_post$text, language="english", 
                      removeStopwords=TRUE, removeNumbers=TRUE, 
                      stemWords = FALSE,removePunctuation = TRUE,toLower = TRUE,stripWhitespace = TRUE) 

data_train_test_post_m <- as.matrix(data_train_test_post_m)

#budowa modelu(random forest), model dopasowany tylko na czesci danych ze wzgledu na czas, full
#classifier_full <- randomForest(data_train_test_post_m[1:3000,], as.factor(data_train_test_post$klasa[1:3000]) )

#saveRDS(object = classifier_full, file = "rf_fulldata.rds")

rf <- readRDS("rf_fulldata.rds")

predicted <- predict(rf, data_train_test_post_m[3000:4000,])
table_full_model <- table(data_train_test_post[3000:4000,"klasa"], predicted)
confusionMatrix_full_model <- confusionMatrix(as.factor(data_train_test_post[3000:4000,"klasa"]), as.factor(predicted))
predict(rf )

#na zbiorze testowym:
#Accuracy : 0.8492,  Kappa : 0.6983, Sensitivity : 0.8646, Specificity : 0.8349

###FETURE EXTRACTION
var <- apply(data_train_test_post_m, 2, var)
data_train_test_post_m <- data_train_test_post_m[,var > 0.001]
#rf_high_var <- randomForest(data_train_test_post_m[1:3000,], as.factor(data_train_test_post$klasa[1:3000]) )

#saveRDS(rf_high_var,"rf_high_var.rds")

rf_high_var <- readRDS("rf_high_var.rds")

pred_high_var <- predict(rf_high_var,data_train_test_post_m[3000:4000,])
confMatr_high_var <- confusionMatrix(as.factor(data_train_test_post[3000:4000,"klasa"]), as.factor(pred_high_var))
#tutaj mamy:
#Accuracy : 0.8541, Kappa : 0.7083, Sensitivity : 0.8675, Specificity : 0.8417
#uwzgledniajac  to ¿e zloznosc obliczeniowa jest duzo mniejsza
#uwazam ze powinniœmy stosowaæ ten model, ale sproboje go ulepszyc, poprzez wybor zmiennych

####knn
#knn <- knn(train = data_train_test_post_m[1:3000,],test = data_train_test_post_m[3001:4000,],cl = data_train_test_post$klasa[1:3000], k = 7)
#saveRDS(object = knn, file = "knn_coffee.rds")
knn_pred <- readRDS("knn_coffee.rds")

confusionMatrix(as.factor(data_train_test_post[3001:4000,"klasa"]), as.factor(knn_pred))
#accuracy 64%, kappa 27.8%, Sensitivity : 0.6894, Specificity : 0.6098
#wiec model odrzucamy



########################WPROWADZANIE NOWYCH DANYCH I OCENA PRZEZ MODEL
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

data <- c("i love unicorns", "i hate the world")
words <- colnames(data_train_test_post_m)
new_data_rf <- new_data(data,words)

predict(rf,newdata = new_data_rf)


##########################PREDYKCJA POSTOW

pred_post_coffee <- unname(predict(rf_high_var ,newdata = data_train_test_post_m[data_train_test_post$typ == "post_coffee",]))
table(pred_post_coffee)[[2]]/length(pred_post_coffee)

#WYKRESY
pred_post_coffee_df <- data.frame(pred = pred_post_coffee, creation_data = as.Date(Posts_coffee$CreationDate),
                                  last_activity_day = as.Date(Posts_coffee$LastActivityDate))
pred_post_coffee_df$diff_time <- as.numeric(stri_extract_first_regex(pred_post_coffee_df$last_activity_day - pred_post_coffee_df$creation_data,
                                                                     "[0-9]+"))
pred_post_coffee_df <- pred_post_coffee_df[order(pred_post_coffee_df$creation_data), ]


positive_by_date <- pred_post_coffee_df %>% mutate(month = stri_datetime_format(creation_data, "yyyy-MM")) %>%  group_by(month) %>% 
  summarize(pozytywne = sum(pred == "positive"), poz_procent = sum(pred == "positive")/n(), count = n())

positive_by_date_melt <- melt(data = positive_by_date[,c("count","pozytywne","month")], id.vars = "month")

ggplot(positive_by_date_melt,aes(x = month, y = value, fill = variable)) + geom_bar(stat="identity",position=position_dodge())+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))





par(mfrow = c(2,1))
x <- barplot(as.matrix(t(positive_by_date[,c("count","pozytywne")])), las = 1,col = c("red","lightblue"), xlab = "Month",beside = TRUE,
             main = "Posts by month", ylim = c(0,300))
box()
legend("topright", legend = c("All posts", "Positive"), fill = c("red","lightblue"))
axis(1, labels = positive_by_date$month[c(1,9,18,27,36,45)], at = (x[1,c(1,9,18,27,36,45)]+x[2,c(1,9,18,27,36,45)])/2 )


y <- barplot(positive_by_date$poz_procent, xlab = "Month", ylim = c(0,1.1),
             col = "tan", main = "% of positive posts", las = 1)
box()
axis(1, labels = positive_by_date$month[c(1,9,18,27,36,45)], at = y[c(1,9,18,27,36,45),1] )
