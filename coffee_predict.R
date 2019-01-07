#dane do pobrania tutaj: ai.stanford.edu/~amaas/data/sentiment/aclImdb_v1.tar.gz
#source("df_from_xml.R")
options(stringsAsFactors = FALSE)
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

#test trzeba wczytaæ zeby testowaæ na nim potem
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
# data_train <- data.frame(text = train$text)

clean_text <- function(text){
  text <- lemmatize_strings(text)
  text <- removePunctuation(text)
  text <- tolower(text)
  text <- removeWords(text,stopwords("en"))
  text <- stripWhitespace(text)
  text
}


#data_train_test_post_m <- readRDS("data_train_test_post_m.rds")

# data_train$text <- clean_text(data_train$text)
# 
# data_train_m <- create_matrix(data_train$text, language="english", 
#                       removeStopwords=TRUE, removeNumbers=TRUE, 
#                       stemWords = FALSE,removePunctuation = TRUE,toLower = TRUE,stripWhitespace = TRUE) 
# 
# data_train_m <- as.matrix(data_train_m)
# saveRDS(data_train_m,"data_train_m.rds")

data_train_m <- readRDS("data_train_m.rds")

#budowa modelu(random forest), full
#classifier_full <- randomForest(data_train_m, as.factor(train$klasa))

#saveRDS(object = classifier_full, file = "rf_fulldata.rds")

rf <- readRDS("rf_fulldata.rds")




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
words <- colnames(data_train_m)
new_data_rf <- new_data(test$text,words)


predicted <- predict(rf,newdata = new_data_rf)
table_full_model <- table(test$klasa, predicted)
confusionMatrix_full_model <- confusionMatrix(as.factor(test$klasa), as.factor(predicted))
#Accuracy : 0.872, Kappa : 0.744, Sensitivity : 0.8735, Specificity : 0.8705



###FETURE EXTRACTION
var <- apply(data_train_m, 2, var)
data_train_m <- data_train_m[,var > 0.001]
words <- colnames(data_train_m)

#rf_high_var <- randomForest(data_train_m, as.factor(train$klasa))

#saveRDS(rf_high_var,"rf_high_var.rds")

rf_high_var <- readRDS("rf_high_var.rds")

pred_high_var <- predict(rf_high_var,new_data_rf)
confMatr_high_var <- confusionMatrix(as.factor(test$klasa), as.factor(pred_high_var))
#tutaj mamy:
#Accuracy : 0.859, Kappa : 0.718, Sensitivity : 0.8472, Specificity : 0.8716
#PYTANIE
#KORZYSTAMY Z TEGO MODELU CZY POPRZEDNIEGO?





##########################PREDYKCJA POSTOW

#########COFFEE

##POSTY
Posts_coffee <- read.csv("data/coffee.stackexchange.com/Posts.csv")

post_coffee_matrix <- new_data(x = Posts_coffee$Body, names = colnames(data_train_m))


pred_post_coffee <- unname(predict(rf_high_var ,newdata = post_coffee_matrix))

df_positive <- data.frame(proc = table(pred_post_coffee)[[2]]/length(pred_post_coffee), typ = "coffee_post", klasa = "coffee")

      

#WYKRESY
pred_post_coffee_df <- data.frame(pred = pred_post_coffee, creation_data = as.Date(Posts_coffee$CreationDate),
                                  last_activity_day = as.Date(Posts_coffee$LastActivityDate))
pred_post_coffee_df$diff_time <- as.numeric(stri_extract_first_regex(pred_post_coffee_df$last_activity_day - pred_post_coffee_df$creation_data,
                                                                     "[0-9]+"))
pred_post_coffee_df <- pred_post_coffee_df[order(pred_post_coffee_df$creation_data), ]


positive_by_date_coffee <- pred_post_coffee_df %>% mutate(month = stri_datetime_format(creation_data, "yyyy-MM")) %>%  group_by(month) %>% 
  summarize(positive = sum(pred == "positive"), poz_procent = sum(pred == "positive")/n(), count = n())

positive_coffee_by_date_melt <- melt(data = positive_by_date_coffee[,c("count","positive","month")], id.vars = "month")

ggplot(positive_coffee_by_date_melt,aes(x = month, y = value, fill = variable)) + geom_bar(stat="identity",position=position_dodge())+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(positive_by_date_coffee,aes(x = month, y = poz_procent)) + geom_bar(stat="identity",position=position_dodge())+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

##KOMENTARZE

Comments_coffee <- read.csv("data/coffee.stackexchange.com/Comments.csv")

comm_coffee_matrix <- new_data(x = Comments_coffee$Text, names = colnames(data_train_m))


pred_comm_coffee <- unname(predict(rf_high_var ,newdata = comm_coffee_matrix))
df_positive <- rbind(df_positive,c(table(pred_comm_coffee)[[2]]/length(pred_comm_coffee),"coffee_comm","coffee"))

#WYKRESY
pred_comm_coffee_df <- data.frame(pred = pred_comm_coffee, creation_data = as.Date(Comments_coffee$CreationDate)) 
                                  
pred_comm_coffee_df <- pred_comm_coffee_df[order(pred_comm_coffee_df$creation_data), ]


positive_by_date_coffee <- pred_comm_coffee_df %>% mutate(month = stri_datetime_format(creation_data, "yyyy-MM")) %>%  group_by(month) %>% 
  summarize(positive = sum(pred == "positive"), poz_procent = sum(pred == "positive")/n(), count = n())

positive_coffee_by_date_melt <- melt(data = positive_by_date_coffee[,c("count","positive","month")], id.vars = "month")

ggplot(positive_coffee_by_date_melt,aes(x = month, y = value, fill = variable)) + geom_bar(stat="identity",position=position_dodge())+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(positive_by_date_coffee,aes(x = month, y = poz_procent)) + geom_bar(stat="identity",position=position_dodge())+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#####BEER

##POSTY
Posts_beer <- read.csv("data/beer.stackexchange.com/Posts.csv")

post_matrix_beer <- new_data(x = Posts_beer$Body, names = colnames(data_train_m))


pred_post_beer <- unname(predict(rf_high_var ,newdata = post_matrix_beer))
df_positive <- rbind(df_positive,c(table(pred_post_beer)[[2]]/length(pred_post_beer),"beer_post","beer"))

#WYKRESY
pred_post_beer_df <- data.frame(pred = pred_post_beer, creation_data = as.Date(Posts_beer$CreationDate),
                                  last_activity_day = as.Date(Posts_beer$LastActivityDate))
pred_post_beer_df$diff_time <- as.numeric(stri_extract_first_regex(pred_post_beer_df$last_activity_day - pred_post_beer_df$creation_data,
                                                                     "[0-9]+"))
pred_post_beer_df <- pred_post_beer_df[order(pred_post_beer_df$creation_data), ]


positive_by_date_beer <- pred_post_beer_df %>% mutate(month = stri_datetime_format(creation_data, "yyyy-MM")) %>%  group_by(month) %>% 
  summarize(positive = sum(pred == "positive"), poz_procent = sum(pred == "positive")/n(), count = n())

positive_beer_by_date_melt <- melt(data = positive_by_date_beer[,c("count","positive","month")], id.vars = "month")

ggplot(positive_beer_by_date_melt,aes(x = month, y = value, fill = variable)) + geom_bar(stat="identity",position=position_dodge())+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(positive_by_date_beer,aes(x = month, y = poz_procent)) + geom_bar(stat="identity",position=position_dodge())+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

##KOMENTARZE

Comments_beer <- read.csv("data/beer.stackexchange.com/Comments.csv")

comm_beer_matrix <- new_data(x = Comments_beer$Text, names = colnames(data_train_m))


pred_comm_beer <- unname(predict(rf_high_var ,newdata = comm_beer_matrix))
df_positive <- rbind(df_positive, c(table(pred_comm_beer)[[2]]/length(pred_comm_beer),"beer_comm","beer"))

#WYKRESY
pred_comm_beer_df <- data.frame(pred = pred_comm_beer, creation_data = as.Date(Comments_beer$CreationDate)) 

pred_comm_beer_df <- pred_comm_beer_df[order(pred_comm_beer_df$creation_data), ]


positive_by_date_beer <- pred_comm_beer_df %>% mutate(month = stri_datetime_format(creation_data, "yyyy-MM")) %>%  group_by(month) %>% 
  summarize(positive = sum(pred == "positive"), poz_procent = sum(pred == "positive")/n(), count = n())

positive_beer_by_date_melt <- melt(data = positive_by_date_beer[,c("count","positive","month")], id.vars = "month")

ggplot(positive_beer_by_date_melt,aes(x = month, y = value, fill = variable)) + geom_bar(stat="identity",position=position_dodge())+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(positive_by_date_beer,aes(x = month, y = poz_procent)) + geom_bar(stat="identity",position=position_dodge())+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


##POSTY
Posts_gaming <- read.csv("data/gaming.stackexchange.com/Posts.csv")

rows_number <- sample(x = 1:nrow(Posts_gaming),size = 5000,replace = FALSE)

post_matrix_gaming <- new_data(x = Posts_gaming$Body[rows_number], names = colnames(data_train_m))


pred_post_gaming <- unname(predict(rf_high_var ,newdata = post_matrix_gaming))
df_positive <- rbind(df_positive,c(table(pred_post_gaming)[[2]]/length(pred_post_gaming),"gaming_post","gaming"))

#WYKRESY
pred_post_gaming_df <- data.frame(pred = pred_post_gaming, creation_data = as.Date(Posts_gaming$CreationDate[rows_number]),
                                last_activity_day = as.Date(Posts_gaming$LastActivityDate[rows_number]))
pred_post_gaming_df$diff_time <- as.numeric(stri_extract_first_regex(pred_post_gaming_df$last_activity_day - pred_post_gaming_df$creation_data,
                                                                   "[0-9]+"))
pred_post_gaming_df <- pred_post_gaming_df[order(pred_post_gaming_df$creation_data), ]


positive_by_date_gaming <- pred_post_gaming_df %>% mutate(month = stri_datetime_format(creation_data, "yyyy-MM")) %>%  group_by(month) %>% 
  summarize(positive = sum(pred == "positive"), poz_procent = sum(pred == "positive")/n(), count = n())

positive_gaming_by_date_melt <- melt(data = positive_by_date_gaming[,c("count","positive","month")], id.vars = "month")

ggplot(positive_gaming_by_date_melt,aes(x = month, y = value, fill = variable)) + geom_bar(stat="identity",position=position_dodge())+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(positive_by_date_gaming,aes(x = month, y = poz_procent)) + geom_bar(stat="identity",position=position_dodge())+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

##KOMENTARZE

Comments_gaming <- read.csv("data/gaming.stackexchange.com/Comments.csv")

rows_number <- sample(x = 1:nrow(Comments_gaming),size = 5000,replace = FALSE)

comm_gaming_matrix <- new_data(x = Comments_gaming$Text[rows_number], names = colnames(data_train_m))


pred_comm_gaming <- unname(predict(rf_high_var ,newdata = comm_gaming_matrix))
df_positive <- rbind(df_positive,c(table(pred_comm_gaming)[[2]]/length(pred_comm_gaming),"gaming_comm","gaming"))

#WYKRESY
pred_comm_gaming_df <- data.frame(pred = pred_comm_gaming, creation_data = as.Date(Comments_gaming$CreationDate[rows_number])) 

pred_comm_gaming_df <- pred_comm_gaming_df[order(pred_comm_gaming_df$creation_data), ]


positive_by_date_gaming <- pred_comm_gaming_df %>% mutate(month = stri_datetime_format(creation_data, "yyyy-MM")) %>%  group_by(month) %>% 
  summarize(positive = sum(pred == "positive"), poz_procent = sum(pred == "positive")/n(), count = n())

positive_gaming_by_date_melt <- melt(data = positive_by_date_gaming[,c("count","positive","month")], id.vars = "month")

ggplot(positive_gaming_by_date_melt,aes(x = month, y = value, fill = variable)) + geom_bar(stat="identity",position=position_dodge())+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(positive_by_date_gaming,aes(x = month, y = poz_procent)) + geom_bar(stat="identity",position=position_dodge())+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#do dopracowania
ggplot(df_positive,aes(x = typ, y = proc, fill = klasa)) + geom_bar(stat="identity",position=position_dodge())+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))




