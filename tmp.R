source("df_from_xml.R")
options(stringsAsFactors = FALSE)
library(ggmap)
library(stringi)
library(maps)
<<<<<<< HEAD
library(dplyr)
=======

>>>>>>> refs/remotes/origin/master
# Wszystkie dane mam wypakowane w folderze data
Comments_beer <- xml_data_frame("data/beer.stackexchange.com/Comments.xml")
Badges_beer <- xml_data_frame("data/beer.stackexchange.com/Badges.xml")
PostsHistory_beer <- xml_data_frame("data/beer.stackexchange.com/PostHistory.xml")
Posts_beer <- xml_data_frame("data/beer.stackexchange.com/Posts.xml")
Users_beer <- xml_data_frame("data/beer.stackexchange.com/Users.xml")
Tags_beer <- xml_data_frame("data/beer.stackexchange.com/Tags.xml")
Votes_beer <- xml_data_frame("data/beer.stackexchange.com/Votes.xml")

<<<<<<< HEAD
Comments_gaming <- xml_data_frame("data/gaming.stackexchange.com/Comments.xml")
Badges_gaming <- xml_data_frame("data/gaming.stackexchange.com/Badges.xml")
PostsHistory_gaming <- xml_data_frame("data/gaming.stackexchange.com/PostHistory.xml")
Posts_gaming <- xml_data_frame("data/gaming.stackexchange.com/Posts.xml")
Users_gaming <- xml_data_frame("data/gaming.stackexchange.com/Users.xml")
Tags_gaming <- xml_data_frame("data/gaming.stackexchange.com/Tags.xml")
Votes_gaming <- xml_data_frame("data/gaming.stackexchange.com/Votes.xml")
=======
Comments_poker <- xml_data_frame("data/poker.stackexchange.com/Comments.xml")
Badges_poker <- xml_data_frame("data/poker.stackexchange.com/Badges.xml")
PostsHistory_poker <- xml_data_frame("data/poker.stackexchange.com/PostHistory.xml")
Posts_poker <- xml_data_frame("data/poker.stackexchange.com/Posts.xml")
Users_poker <- xml_data_frame("data/poker.stackexchange.com/Users.xml")
Tags_poker <- xml_data_frame("data/poker.stackexchange.com/Tags.xml")
Votes_poker <- xml_data_frame("data/poker.stackexchange.com/Votes.xml")
>>>>>>> refs/remotes/origin/master

Comments_coffee <- xml_data_frame("data/coffee.stackexchange.com/Comments.xml")
Badges_coffee <- xml_data_frame("data/coffee.stackexchange.com/Badges.xml")
PostsHistory_coffee <- xml_data_frame("data/coffee.stackexchange.com/PostHistory.xml")
Posts_coffee <- xml_data_frame("data/coffee.stackexchange.com/Posts.xml")
Users_coffee <- xml_data_frame("data/coffee.stackexchange.com/Users.xml")
Tags_coffee <- xml_data_frame("data/coffee.stackexchange.com/Tags.xml")
Votes_coffee <- xml_data_frame("data/coffee.stackexchange.com/Votes.xml")

lokacje <- function(lokalizacje){
  location <- strsplit(lokalizacje[stri_detect_regex(lokalizacje,",")], split = ",")
  location <- location[!is.na(location)]
<<<<<<< HEAD
  lokalizacje <- matrix(unlist(lapply(location,function(x) geocode(x[1],source = "dsk"))),ncol = 2,byrow = TRUE)
=======
  lokalizacje <- matrix(unlist(lapply(location,function(x) geocode(x[1],source = "google"))),ncol = 2,byrow = TRUE)
>>>>>>> refs/remotes/origin/master
  lokalizacje <- as.data.frame(lokalizacje)
  colnames(lokalizacje) <- c("lon","lan")
  lokalizacje
}

save_locations <- function(Users){
  save_name <- deparse(substitute(Users))
  save_name <- paste(save_name, "location", sep="_")
  file_name <- paste(save_name, "rds", sep=".")
<<<<<<< HEAD
  print(file_name)
  if(!file.exists(file_name)){
    lokalizacje <- lokacje(Users$Location)
    
    saveRDS(lokalizacje, file=file_name)
=======
  file_path <- file.path("saved", file_name)
  print(file_path)
  if(!file.exists(file_path)){
    lokalizacje <- lokacje(Users$Location)
    
    saveRDS(lokalizacje, file=file_path)
>>>>>>> refs/remotes/origin/master
  } else{
    print("Locations already saved")
  }
}

<<<<<<< HEAD
#beer
#lokacje_beer <- lokacje(Users_beer$Location)
#save_locations(Users_beer)
#saveRDS(lokacje_beer, file="Users_beer_location.rds")

=======
save_locations(Users_beer)
>>>>>>> refs/remotes/origin/master
locations_beers <- readRDS("Users_beer_location.rds")

par(mar=c(0,0,0,0))
map('world',col="#f2f2f2", fill=TRUE, bg="white", lwd=0.05,mar=rep(0,4),border=0, ylim=c(-80,80),main ="mapa")
<<<<<<< HEAD
points(locations_beers$lon,locations_beers$lan,pch = 16,cex = 0.7, col = rgb(red = 0, green = 0, blue = 0, alpha = 0.2))

#coffee
#lokalizacje <- lokacje(Users_beer$Location)
#saveRDS(lokalizacje, file="Users_coffee_location.rds")
locations_coffee <- readRDS("Users_coffee_location.rds")

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

#Ranking miast
miasta_najwieksza_ilosc_pkt <- najw_pkt %>% group_by(Location) %>% summarise(suma = sum(pkt)) %>% arrange(desc(suma)) %>% na.omit()
miasta_najaktywniejsi_uzytkownicy <- najaktywniejsi_uzytkownicy %>% group_by(Location) %>% summarise(suma = sum(ilosc)) %>%
  arrange(desc(suma)) %>% na.omit()
miasta_najaktywniejsi_uzytkownicy[1:10,]
miasta_najwieksza_ilosc_pkt[1:10,]



#gaming
#saveRDS(lokacje(Users_gaming$Location), file="Users_gaming_location.rds")

locations_gaming <- readRDS("Users_gaming_location.rds")

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


=======
points(lokalizacje$lon,lokalizacje$lan,pch = 16,cex = 0.7, col = rgb(red = 0, green = 0, blue = 0, alpha = 0.2))
>>>>>>> refs/remotes/origin/master

