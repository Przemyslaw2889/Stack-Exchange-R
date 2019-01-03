source("df_from_xml.R")
options(stringsAsFactors = FALSE)
setwd("coffee.stackexchange.com/") 

Comments <- read.csv("data/coffee.stackexchange.com/Comments.csv")
Badges <- read.csv("data/coffee.stackexchange.com/Badges.csv")
PostsHistory <- read.csv("data/coffee.stackexchange.com/PostHistory.csv")
Posts <- read.csv("data/coffee.stackexchange.com/Posts.csv")
Users <- read.csv("data/coffee.stackexchange.com/Users.csv")
Tags <- read.csv("data/coffee.stackexchange.com/Tags.csv")
Votes <- read.csv("data/beer.stackexchange.com/Votes.csv")

#biblioteki
library(ggmap)
library(stringi)
library(tidytext)
library(dplyr)
library(maps)
library(tidytext)
library(tm)
library(wordcloud)
library(qdap)
library(RWeka)
#Histogramy
Votes_czas <- as.numeric(format(as.Date(Votes$CreationDate, "%Y"),"%Y"))
Posts_czas <- as.numeric(format(as.Date(Posts$CreationDate, "%Y"),"%Y"))
Comments_czas <- as.numeric(format(as.Date(Comments$CreationDate, "%Y"),"%Y"))

par(mar = c(5,4.5,2.1,2))
hist(Votes_czas,breaks = c(2014:2018),col = "lightblue",main = "Histogram liczby lajk?w w latach",xlab= "Rok",
     ylab = "Liczba lajk?w",las=1)
box()
hist(Posts_czas,breaks = c(2014:2018),col = "lightblue",main = "Histogram liczby dodawanych post?w w latach",xlab= "Rok",
     ylab = "Liczba dodanych post?w",las=1)
box()
hist(Comments_czas,breaks = c(2014:2018),col = "lightblue",main = "Histogram liczby dodawanych komentarzy w latach",xlab= "Rok",
     ylab = "Liczba dodanych komentarzy",las=1)
box()

frequency <- freq_terms(Comments$Text, top = 10, stopwords = stopwords("en"))
plot(frequency)

#text-mining (baaaardzo elementarny), dany komentarz otrzymuje tak? emocje ktorej wiecej slow jest w jego tresci
df_on_list <- function(x){
  x <- data.frame(x)
  colnames(x) <- "word"
  x
}


list <- as.list(tolower(Comments$Text))
list <- lapply(list,stri_extract_all_regex,"[a-z]+")
list <- lapply(list,df_on_list)
list <- lapply(list, function(x){
  x %>%
    inner_join(data.frame(get_sentiments("nrc")),by = "word") %>% 
    group_by(sentiment) %>% count(sentiment, sort = TRUE) %>% as.data.frame() %>% top_n(1)
  })

from_df_to_vector <- function(x){
  vec <- x[,2]
  names(vec) <- x[,1]
  vec
}

sentiment_vector <- unlist(lapply(list, from_df_to_vector))
#emocje
df_sentiment <- data.frame(moc = sentiment_vector, emocja = names(sentiment_vector))
df_sentiment <- df_sentiment %>% group_by(emocja) %>%
  summarise(suma = sum(moc), liczba = n())

par(oma = c(0,0,0,0))
par(mar = c(6,4,3,2))
plot(df_sentiment$suma, x = 1:10, type = "h", axes = FALSE,
     ylab = '', col = "red",xlab = "", lwd = 2)
axis(2,las = 1)
axis(1,at = 1:10,labels = df_sentiment$names.sentiment_vector., las = 2)
box()
for (i in 1:length(df_sentiment$liczba)){
  lines(c(i+0.2,i+0.2),c(0,df_sentiment$liczba[i]),lwd=2)
}

legend(x = 0.63, y = 5350,col = c("red","black"), legend = c("liczba","suma"),lty = 1,lwd = 2)

#positive
pos_comment <- unnest_tokens(tbl = Comments,input = Text, output = word,to_lower = TRUE)[,c("Score","word","Id")] %>% 
  inner_join(get_sentiments("bing")) %>% filter(sentiment == "positive") %>% group_by(word) %>% summarise(count = n()) %>% 
  arrange(desc(count))


#negative
neg_comment <- unnest_tokens(tbl = Comments,input = Text, output = word,to_lower = TRUE)[,c("Score","word","Id")] %>% 
  inner_join(get_sentiments("bing")) %>% filter(sentiment == "negative") %>% group_by(word) %>% summarise(count = n()) %>% 
  arrange(desc(count))

top_neg <- data.frame(count = -neg_comment$count[1:10],word = neg_comment$word[1:10])
top_pos <- pos_comment[1:10,]

top_word <- top_neg %>% union(top_pos) %>% arrange(count)
barplot(top_word$count,col= c(rep("lightblue",10),rep("red",10)), las = 1, ylim = c(-220,450))
axis(1,labels = top_word$word, at = seq(1,25,length.out = 20), las = 2)
box()

#polarity

polarity_comments <-  polarity(
  text.var       = Comments$Text,
  grouping.var   = Comments$Id,
  polarity.frame = key.pol,
  negators       = negation.words,
  amplifiers     = amplification.words,
  deamplifiers   = deamplification.words 
)

length(unique(Comments$PostId))


polarity_by_posts <-  polarity(
  text.var       = Comments$Text,
  grouping.var   = Comments$PostId,
  polarity.frame = key.pol,
  negators       = negation.words,
  amplifiers     = amplification.words,
  deamplifiers   = deamplification.words 
)


polarity_by_posts$group[1:5,]
polarity_comments$group[1:5,]

polarity_by_user <-  polarity(
  text.var       = Comments$Text,
  grouping.var   = Comments$UserId,
  polarity.frame = key.pol,
  negators       = negation.words,
  amplifiers     = amplification.words,
  deamplifiers   = deamplification.words 
)

par(mfrow = c(1,3))
boxplot(polarity_comments$group$ave.polarity,pch = 16,main = "on every comment", col = "ivory2")
boxplot(polarity_by_posts$group$ave.polarity,pch = 16, main = "by posts", col = "ivory3")
boxplot(polarity_by_user$group$ave.polarity,pch = 16, main = "by User", col = "ivory4")


#word cloud
clean_corpus <- function(corpus) {
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, c(stopwords("en"),"coffee"))
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}#czysczenie danych ze znakow interpunkcyjnych, usuwuanie tzw. stopwords ("the", "I" itd), zmiana wszystkiego na male listery oraz
#usuwanie bialych spacji

Comm_corp <- VCorpus(VectorSource(Comments$Text))
Comm_corp <- clean_corpus(Comm_corp)

tdm_comm <- as.matrix(TermDocumentMatrix(Comm_corp))
freq_word <- rowSums(tdm_comm)
freq_word <- sort(freq_word, decreasing = TRUE)

wordcloud(names(freq_word),freq_word ,max.words = 20, col = "tan")

Post_corp <- clean_corpus(VCorpus(VectorSource(Posts$Body)))
tdm_post <- as.matrix(TermDocumentMatrix(Post_corp))
freq_word_post <- sort(rowSums(tdm_post), decreasing = TRUE)

wordcloud(names(freq_word_post),freq_word_post ,max.words = 20, col = "tan")

#bigramy
#komentarze
tokenizer <- function(x){
  NGramTokenizer(x, Weka_control(min=2, max = 2) )
}

bigram_dtm_comm <- as.matrix(TermDocumentMatrix(Comm_corp,control = list(tokenize = tokenizer) ))
bigram_freq_comm <- sort(rowSums(bigram_dtm_comm), decreasing = TRUE)

wordcloud(names(bigram_freq_comm),bigram_freq_comm,max.words = 20, col = "tan")

#tresc postow
bigram_dtm_post <- as.matrix(TermDocumentMatrix(Post_corp,control = list(tokenize = tokenizer) ))
bigram_freq_post <- sort(rowSums(bigram_dtm_post), decreasing = TRUE)

wordcloud(names(bigram_freq_post),bigram_freq_post,max.words = 20, col = "tan")

#Popularnosc kawy
#komentarze
popular_coffee <- c("americano","latte", "cappuccino", "flat white", "long black", "mocchiato",
                   "piccolo latte","mochaccino","irish coffee","vienna","affogato","espresso")
clean_coments <- lapply(Comm_corp, "[",1)
coffee_type_comm <- lapply(clean_coments, stri_detect_fixed, popular_coffee)
coffee_type_comm <- as.data.frame(t(as.data.frame(coffee_type_comm)))
colnames(coffee_type_comm) <- popular_coffee
sum_coffee_type_comm <- colSums(coffee_type_comm)

#tresc postow
clean_posts_body <- lapply(Post_corp, "[",1)
coffee_type_posts_body <- lapply(clean_posts_body, stri_detect_fixed, popular_coffee)
coffee_type_posts_body <- as.data.frame(t(as.data.frame(coffee_type_posts_body)))
colnames(coffee_type_posts_body) <- popular_coffee
sum_coffee_type_posts <- colSums(coffee_type_posts_body)

df_coffee_type <- data.frame(count = c(sum_coffee_type_comm,sum_coffee_type_posts), type = c(rep("comm",12),rep("body post",12)),
                             coffee =  rep(popular_coffee,2))


ggplot(df_coffee_type,aes(y = count, x = reorder(coffee,-count), fill = type)) +
  geom_bar(stat="identity",position=position_dodge()) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_brewer(type = "seq",palette = "OrRd") +
  geom_text(aes(label=count), vjust=1.6, color="black",
            position = position_dodge(0.9), size=3.5)
