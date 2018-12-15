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
lokacje <- function(lokalizacje){
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

legend(x = -183, y = -55,col = c("red","blue"), legend = c("aktywny uzytkownik","duz¹ liczb¹ lajków"),pch = 16)
#Ranking miast
miasta_najwieksza_ilosc_pkt <- najw_pkt %>% group_by(Location) %>% summarise(suma = sum(pkt)) %>% arrange(desc(suma)) %>% na.omit()
miasta_najaaktywniejsi_uzytkownicy <- najaktywniejsi_urzytkownicy %>% group_by(Location) %>% summarise(suma = sum(ilosc)) %>%
  arrange(desc(suma)) %>% na.omit()
miasta_najaaktywniejsi_uzytkownicy[1:10,]
miasta_najwieksza_ilosc_pkt[1:10,]




