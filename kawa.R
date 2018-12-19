source("df_from_xml.R")
options(stringsAsFactors = FALSE)
setwd("coffee.stackexchange.com/")

Comments <- xml_data_frame("Comments.xml")
Badges <- xml_data_frame("Badges.xml")
PostsHistory <- xml_data_frame("PostHistory.xml")
Posts <- xml_data_frame("Posts.xml")
Users <- xml_data_frame("Users.xml")
Tags <- xml_data_frame("Tags.xml")
Votes <- xml_data_frame("Votes.xml")

#mapa
library(ggmap)
library(stringi)
library(tidytext)lokacje <- function(lokalizacje){
  location <- strsplit(lokalizacje[stri_detect_regex(lokalizacje,",")], split = ",")
  location <- location[!is.na(location)]
  lokalizacje <- matrix(unlist(lapply(location,function(x) geocode(x[1],source = "dsk"))),ncol = 2,byrow = TRUE)
  lokalizacje <- as.data.frame(lokalizacje)
  colnames(lokalizacje) <- c("lon","lan")
  lokalizacje
  }

lokalizacje <- lokacje(Users$Location)
#install.packages("maps")
library(maps)
par(mar=c(0,0,0,0))
map('world',col="#f2f2f2", fill=TRUE, bg="white", lwd=0.05,mar=rep(0,4),border=0, ylim=c(-80,80),main ="mapa")
points(lokalizacje$lon,lokalizacje$lan,pch = 16,cex = 0.7, col = rgb(red = 0, green = 0, blue = 0, alpha = 0.2))

library(dplyr)
najaktywniejsi_uzytkownicy <- Posts %>% group_by(OwnerUserId) %>% summarise(ilosc = n()) %>% 
  left_join(Users,by = c("OwnerUserId" = "Id")) %>% select(ilosc,OwnerUserId, Location) %>% arrange(desc(ilosc)) %>% as.data.frame()

lok_naj_aktyw <- lokacje(najaktywniejsi_uzytkownicy[1:20,"Location"])

points(lok_naj_aktyw$lon,lok_naj_aktyw$lan, pch = 16, cex = 0.7, col = "red")

najw_pkt <- Posts %>% group_by(OwnerUserId) %>% summarise(pkt = sum(as.numeric(Score))) %>% 
  left_join(Users,by = c("OwnerUserId" = "Id")) %>% select(pkt,OwnerUserId, Location) %>% arrange(desc(pkt)) %>% as.data.frame()

lok_najw_pkt <- lokacje(najw_pkt$Location[1:10])

points(lok_najw_pkt$lon,lok_najw_pkt$lan, pch = 16, cex = 0.7, col = "blue")

legend(x = -183, y = -53,col = c("red","blue"), legend = c("aktywny uzytkownik","duz¹ liczb¹ lajków"),pch = 16)
#Ranking miast
miasta_najwieksza_ilosc_pkt <- najw_pkt %>% group_by(Location) %>% summarise(suma = sum(pkt)) %>% arrange(desc(suma)) %>% na.omit()
miasta_najaaktywniejsi_uzytkownicy <- najaktywniejsi_urzytkownicy %>% group_by(Location) %>% summarise(suma = sum(ilosc)) %>%
  arrange(desc(suma)) %>% na.omit()
miasta_najaaktywniejsi_uzytkownicy[1:10,]
miasta_najwieksza_ilosc_pkt[1:10,]
#Histogramy
Votes_czas <- as.numeric(format(as.Date(Votes$CreationDate, "%Y"),"%Y"))
Posts_czas <- as.numeric(format(as.Date(Posts$CreationDate, "%Y"),"%Y"))
Comments_czas <- as.numeric(format(as.Date(Comments$CreationDate, "%Y"),"%Y"))

par(mar = c(5,4.5,2.1,2))
hist(Votes_czas,breaks = c(2014:2018),col = "lightblue",main = "Histogram liczby lajków w latach",xlab= "Rok",
     ylab = "Liczba lajków",las=1)
box()
hist(Posts_czas,breaks = c(2014:2018),col = "lightblue",main = "Histogram liczby dodawanych postów w latach",xlab= "Rok",
     ylab = "Liczba dodanych postów",las=1)
box()
hist(Comments_czas,breaks = c(2014:2018),col = "lightblue",main = "Histogram liczby dodawanych komentarzy w latach",xlab= "Rok",
     ylab = "Liczba dodanych komentarzy",las=1)
box()

#text-mining (baaaardzo elementarny)
library(tidytext)
get_sentiments("afinn")
df_on_list <- function(x){
  x <- data.frame(x)
  colnames(x) <- "word"
  x
}


list <- as.list(Comments$Text)
list <- lapply(list,stri_extract_all_regex,"[A-z]+")
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
df_sentiment <- data.frame(sentiment_vector, names(sentiment_vector))
df_sentiment <- df_sentiment %>% group_by(names.sentiment_vector.) %>%
  summarise(suma = sum(sentiment_vector), liczba = n())

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

legend(x = 0.62, y = 5050,col = c("red","black"), legend = c("liczba","suma"),lty = 1,lwd = 2)

