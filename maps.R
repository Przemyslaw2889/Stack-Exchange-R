source("df_from_xml.R")
options(stringsAsFactors = FALSE)
library(ggmap)
library(stringi)
library(maps)
library(dplyr)

## Data reading
# Wszystkie dane mam wypakowane w folderze data i przekonwertowane na csv

Posts_beer <- read.csv("data/beer.stackexchange.com/Posts.csv")
Users_beer <- read.csv("data/beer.stackexchange.com/Users.csv")

Posts_gaming <- read.csv("data/gaming.stackexchange.com/Posts.csv")
Users_gaming <- read.csv("data/gaming.stackexchange.com/Users.csv")

Posts_coffee <- read.csv("data/coffee.stackexchange.com/Posts.csv")
Users_coffee <- read.csv("data/coffee.stackexchange.com/Users.csv")

lokacje <- function(lokalizacje){
  location <- strsplit(lokalizacje[stri_detect_regex(lokalizacje,",")], split = ",")
  location <- location[!is.na(location)]
  lokalizacje <- matrix(unlist(lapply(location,function(x) geocode(x[1],source = "dsk"))),ncol = 2,byrow = TRUE)
  lokalizacje <- as.data.frame(lokalizacje)
  colnames(lokalizacje) <- c("lon","lan")
  lokalizacje
}

# save_locations <- function(Users){
#   save_name <- deparse(substitute(Users))
#   save_name <- paste(save_name, "location", sep="_")
#   file_name <- paste(save_name, "rds", sep=".")
#   file_path <- file.path("saved", file_name)
#   print(file_path)
#   if(!file.exists(file_path)){
#     lokalizacje <- lokacje(Users$Location)
#     saveRDS(lokalizacje, file=file_path)
#   } else{
#     print("Locations already saved")
#   }
# }

# save_locations(Users_coffee)
locations_coffee <- readRDS(file.path("saved", "Users_coffee_location.rds"))

# save_locations(Users_beer)
locations_beers <- readRDS(file.path("saved", "Users_beer_location.rds"))

# save_locations(Users_gaming)
locations_gaming <- readRDS(file.path("saved", "Users_gaming_location.rds"))


##### BEER
najaktywniejsi_uzytkownicy_beer<- Posts_beer %>% group_by(OwnerUserId) %>% summarise(ilosc = n()) %>% 
  left_join(Users_beer,by = c("OwnerUserId" = "Id")) %>% select(ilosc,OwnerUserId, Location) %>% arrange(desc(ilosc)) %>% as.data.frame()

lok_naj_aktyw_beer <- lokacje(najaktywniejsi_uzytkownicy_beer[1:20,"Location"])

najw_pkt_beer <- Posts_beer %>% group_by(OwnerUserId) %>% summarise(pkt = sum(as.numeric(Score))) %>% 
  left_join(Users_beer,by = c("OwnerUserId" = "Id")) %>% select(pkt,OwnerUserId, Location) %>% arrange(desc(pkt)) %>% as.data.frame()

lok_najw_pkt_beer <- lokacje(najw_pkt_beer$Location[1:10])


##### COFFEE

najaktywniejsi_uzytkownicy <- Posts_coffee %>% group_by(OwnerUserId) %>% summarise(ilosc = n()) %>% 
  left_join(Users_coffee,by = c("OwnerUserId" = "Id")) %>% select(ilosc,OwnerUserId, Location) %>% arrange(desc(ilosc)) %>% as.data.frame()

lok_naj_aktyw <- lokacje(najaktywniejsi_uzytkownicy[1:20,"Location"])

najw_pkt <- Posts_coffee %>% group_by(OwnerUserId) %>% summarise(pkt = sum(as.numeric(Score))) %>% 
  left_join(Users_coffee,by = c("OwnerUserId" = "Id")) %>% select(pkt,OwnerUserId, Location) %>% arrange(desc(pkt)) %>% as.data.frame()

lok_najw_pkt <- lokacje(najw_pkt$Location[1:10])

# #Ranking miast
# miasta_najwieksza_ilosc_pkt <- najw_pkt %>% group_by(Location) %>% summarise(suma = sum(pkt)) %>% arrange(desc(suma)) %>% na.omit()
# miasta_najaktywniejsi_uzytkownicy <- najaktywniejsi_uzytkownicy %>% group_by(Location) %>% summarise(suma = sum(ilosc)) %>%
#   arrange(desc(suma)) %>% na.omit()
# miasta_najaktywniejsi_uzytkownicy[1:10,]
# miasta_najwieksza_ilosc_pkt[1:10,]


##### GAMING

najaktywniejsi_uzytkownicy_gam <- Posts_gaming %>% group_by(OwnerUserId) %>% summarise(ilosc = n()) %>% 
  left_join(Users_coffee,by = c("OwnerUserId" = "Id")) %>% select(ilosc,OwnerUserId, Location) %>% arrange(desc(ilosc)) %>% as.data.frame()

lok_naj_aktyw_gam <- lokacje(najaktywniejsi_uzytkownicy_gam[1:20,"Location"])

najw_pkt_gam <- Posts_gaming %>% group_by(OwnerUserId) %>% summarise(pkt = sum(as.numeric(Score))) %>% 
  left_join(Users_gaming,by = c("OwnerUserId" = "Id")) %>% select(pkt,OwnerUserId, Location) %>% arrange(desc(pkt)) %>% as.data.frame()

lok_najw_pkt_gam <- lokacje(najw_pkt_gam$Location[1:10])

#funkcja do aplikacji

lista_location <- list(coffee = locations_coffee,beer = locations_beers,gaming = locations_gaming)
lista_najw_pkt <- list(coffee = lok_najw_pkt, beer = lok_najw_pkt_beer, gaming = lok_najw_pkt_gam )
lista_najaktyw <- list(beer = lok_naj_aktyw_beer, coffee = lok_naj_aktyw, gaming = lok_naj_aktyw_gam )

mapa <- function(forum){
  par(mar=c(0,0,0,0))
  map('world',col="#f2f2f2", fill=TRUE, bg="white", lwd=0.05,mar=rep(0,4),border=0, ylim=c(-80,80),main ="mapa")
  points(lista_location[[forum]]$lon,lista_location[[forum]]$lan,pch = 16,cex = 0.7, col = rgb(red = 0, green = 0, blue = 0, alpha = 0.2))
  title(forum)
  points(lista_najw_pkt[[forum]]$lon, lista_najw_pkt[[forum]]$lan,pch=16,cex = 0.7,col = "blue")
  points(lista_najaktyw[[forum]]$lon , lista_najaktyw[[forum]]$lan,pch=16,cex = 0.7, col = "red")
  legend(x = -183, y = -53,col = c("red","blue"), legend = c("most active users","users with highest number of likes"),pch = 16)
}
