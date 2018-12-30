source("df_from_xml.R")
options(stringsAsFactors = FALSE)
library(ggmap)
library(stringi)
library(maps)
library(dplyr)

## Data reading
# Wszystkie dane mam wypakowane w folderze data i przekonwertowane na csv
Comments_beer <- read.csv("data/beer.stackexchange.com/Comments.csv")
Badges_beer <- read.csv("data/beer.stackexchange.com/Badges.csv")
PostsHistory_beer <- read.csv("data/beer.stackexchange.com/PostHistory.csv")
Posts_beer <- read.csv("data/beer.stackexchange.com/Posts.csv")
Users_beer <- read.csv("data/beer.stackexchange.com/Users.csv")
Tags_beer <- read.csv("data/beer.stackexchange.com/Tags.csv")
Votes_beer <- read.csv("data/beer.stackexchange.com/Votes.csv")

Comments_gaming <- read.csv("data/gaming.stackexchange.com/Comments.csv")
Badges_gaming <- read.csv("data/gaming.stackexchange.com/Badges.csv")
PostsHistory_gaming <- read.csv("data/gaming.stackexchange.com/PostHistory.csv")
Posts_gaming <- read.csv("data/gaming.stackexchange.com/Posts.csv")
Users_gaming <- read.csv("data/gaming.stackexchange.com/Users.csv")
Tags_gaming <- read.csv("data/gaming.stackexchange.com/Tags.csv")
Votes_gaming <- read.csv("data/gaming.stackexchange.com/Votes.csv")

Comments_coffee <- read.csv("data/coffee.stackexchange.com/Comments.csv")
Badges_coffee <- read.csv("data/coffee.stackexchange.com/Badges.csv")
PostsHistory_coffee <- read.csv("data/coffee.stackexchange.com/PostHistory.csv")
Posts_coffee <- read.csv("data/coffee.stackexchange.com/Posts.csv")
Users_coffee <- read.csv("data/coffee.stackexchange.com/Users.csv")
Tags_coffee <- read.csv("data/coffee.stackexchange.com/Tags.csv")
Votes_coffee <- read.csv("data/coffee.stackexchange.com/Votes.csv")

# Comments_beer <- xml_data_frame("data/beer.stackexchange.com/Comments.xml")
# Badges_beer <- xml_data_frame("data/beer.stackexchange.com/Badges.xml")
# PostsHistory_beer <- xml_data_frame("data/beer.stackexchange.com/PostHistory.xml")
# Posts_beer <- xml_data_frame("data/beer.stackexchange.com/Posts.xml")
# Users_beer <- xml_data_frame("data/beer.stackexchange.com/Users.xml")
# Tags_beer <- xml_data_frame("data/beer.stackexchange.com/Tags.xml")
# Votes_beer <- xml_data_frame("data/beer.stackexchange.com/Votes.xml")
# 
# Comments_gaming <- xml_data_frame("data/gaming.stackexchange.com/Comments.xml")
# Badges_gaming <- xml_data_frame("data/gaming.stackexchange.com/Badges.xml")
# PostsHistory_gaming <- xml_data_frame("data/gaming.stackexchange.com/PostHistory.xml")
# Posts_gaming <- xml_data_frame("data/gaming.stackexchange.com/Posts.xml")
# Users_gaming <- xml_data_frame("data/gaming.stackexchange.com/Users.xml")
# Tags_gaming <- xml_data_frame("data/gaming.stackexchange.com/Tags.xml")
# Votes_gaming <- xml_data_frame("data/gaming.stackexchange.com/Votes.xml")
#
# Comments_coffee <- xml_data_frame("data/coffee.stackexchange.com/Comments.xml")
# Badges_coffee <- xml_data_frame("data/coffee.stackexchange.com/Badges.xml")
# PostsHistory_coffee <- xml_data_frame("data/coffee.stackexchange.com/PostHistory.xml")
# Posts_coffee <- xml_data_frame("data/coffee.stackexchange.com/Posts.xml")
# Users_coffee <- xml_data_frame("data/coffee.stackexchange.com/Users.xml")
# Tags_coffee <- xml_data_frame("data/coffee.stackexchange.com/Tags.xml")
# Votes_coffee <- xml_data_frame("data/coffee.stackexchange.com/Votes.xml")

lokacje <- function(lokalizacje){
  location <- strsplit(lokalizacje[stri_detect_regex(lokalizacje,",")], split = ",")
  location <- location[!is.na(location)]
  lokalizacje <- matrix(unlist(lapply(location,function(x) geocode(x[1],source = "dsk"))),ncol = 2,byrow = TRUE)
  lokalizacje <- as.data.frame(lokalizacje)
  colnames(lokalizacje) <- c("lon","lan")
  lokalizacje
}

save_locations <- function(Users){
  save_name <- deparse(substitute(Users))
  save_name <- paste(save_name, "location", sep="_")
  file_name <- paste(save_name, "rds", sep=".")
  file_path <- file.path("saved", file_name)
  print(file_path)
  if(!file.exists(file_path)){
    lokalizacje <- lokacje(Users$Location)
    saveRDS(lokalizacje, file=file_path)
  } else{
    print("Locations already saved")
  }
}

save_locations(Users_coffee)
locations_coffee <- readRDS(file.path("saved", "Users_coffee_location.rds"))

save_locations(Users_beer)
locations_beers <- readRDS(file.path("saved", "Users_beer_location.rds"))

save_locations(Users_gaming)
locations_gaming <- readRDS(file.path("saved", "Users_gaming_location.rds"))

##### BEER
par(mar=c(0,0,0,0))
map('world',col="#f2f2f2", fill=TRUE, bg="white", lwd=0.05,mar=rep(0,4),border=0, ylim=c(-80,80),main ="mapa")
points(locations_beers$lon,locations_beers$lan,pch = 16,cex = 0.7, col = rgb(red = 0, green = 0, blue = 0, alpha = 0.2))


##### COFFEE
par(mar=c(0,0,0,0))
map('world',col="#f2f2f2", fill=TRUE, bg="white", lwd=0.05,mar=rep(0,4),border=0, ylim=c(-80,80),main ="mapa")
points(locations_coffee$lon,locations_coffee$lan,pch = 16,cex = 0.7, col = rgb(red = 0, green = 0, blue = 0, alpha = 0.2))

najaktywniejsi_uzytkownicy <- Posts_coffee %>% group_by(OwnerUserId) %>% summarise(ilosc = n()) %>% 
  left_join(Users_coffee,by = c("OwnerUserId" = "Id")) %>% select(ilosc,OwnerUserId, Location) %>% arrange(desc(ilosc)) %>% as.data.frame()

lok_naj_aktyw <- lokacje(najaktywniejsi_uzytkownicy[1:20,"Location"])

points(lok_naj_aktyw$lon,lok_naj_aktyw$lan, pch = 16, cex = 0.7, col = "red")

najw_pkt <- Posts_coffee %>% group_by(OwnerUserId) %>% summarise(pkt = sum(as.numeric(Score))) %>% 
  left_join(Users_coffee,by = c("OwnerUserId" = "Id")) %>% select(pkt,OwnerUserId, Location) %>% arrange(desc(pkt)) %>% as.data.frame()

lok_najw_pkt <- lokacje(najw_pkt$Location[1:10])

points(lok_najw_pkt$lon,lok_najw_pkt$lan, pch = 16, cex = 0.7, col = "blue")

legend(x = -183, y = -53,col = c("red","blue"), legend = c("most active users","users with highest number of likes"),pch = 16)
title("Coffee")

#Ranking miast
miasta_najwieksza_ilosc_pkt <- najw_pkt %>% group_by(Location) %>% summarise(suma = sum(pkt)) %>% arrange(desc(suma)) %>% na.omit()
miasta_najaktywniejsi_uzytkownicy <- najaktywniejsi_uzytkownicy %>% group_by(Location) %>% summarise(suma = sum(ilosc)) %>%
  arrange(desc(suma)) %>% na.omit()
miasta_najaktywniejsi_uzytkownicy[1:10,]
miasta_najwieksza_ilosc_pkt[1:10,]


##### GAMING
par(mar=c(0,0,0,0))
map('world',col="#f2f2f2", fill=TRUE, bg="white", lwd=0.05,mar=rep(0,4),border=0, ylim=c(-80,80),main ="mapa")
points(locations_gaming$lon,locations_gaming$lan,pch = 16,cex = 0.7, col = rgb(red = 0, green = 0, blue = 0, alpha = 0.05))


najaktywniejsi_uzytkownicy_gam <- Posts_gaming %>% group_by(OwnerUserId) %>% summarise(ilosc = n()) %>% 
  left_join(Users_coffee,by = c("OwnerUserId" = "Id")) %>% select(ilosc,OwnerUserId, Location) %>% arrange(desc(ilosc)) %>% as.data.frame()

lok_naj_aktyw_gam <- lokacje(najaktywniejsi_uzytkownicy_gam[1:20,"Location"])

points(lok_naj_aktyw_gam$lon,lok_naj_aktyw$lan[1:5], pch = 16, cex = 0.7, col = "red")

najw_pkt_gam <- Posts_gaming %>% group_by(OwnerUserId) %>% summarise(pkt = sum(as.numeric(Score))) %>% 
  left_join(Users_gaming,by = c("OwnerUserId" = "Id")) %>% select(pkt,OwnerUserId, Location) %>% arrange(desc(pkt)) %>% as.data.frame()

lok_najw_pkt_gam <- lokacje(najw_pkt_gam$Location[1:10])

points(lok_najw_pkt$lon,lok_najw_pkt$lan, pch = 16, cex = 0.7, col = "blue")

legend(x = -183, y = -53,col = c("red","blue"), legend = c("most active users","users with highest number of likes"),pch = 16)



