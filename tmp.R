source("df_from_xml.R")
options(stringsAsFactors = FALSE)
library(ggmap)
library(stringi)
library(maps)

# Wszystkie dane mam wypakowane w folderze data
Comments_beer <- xml_data_frame("data/beer.stackexchange.com/Comments.xml")
Badges_beer <- xml_data_frame("data/beer.stackexchange.com/Badges.xml")
PostsHistory_beer <- xml_data_frame("data/beer.stackexchange.com/PostHistory.xml")
Posts_beer <- xml_data_frame("data/beer.stackexchange.com/Posts.xml")
Users_beer <- xml_data_frame("data/beer.stackexchange.com/Users.xml")
Tags_beer <- xml_data_frame("data/beer.stackexchange.com/Tags.xml")
Votes_beer <- xml_data_frame("data/beer.stackexchange.com/Votes.xml")

Comments_poker <- xml_data_frame("data/poker.stackexchange.com/Comments.xml")
Badges_poker <- xml_data_frame("data/poker.stackexchange.com/Badges.xml")
PostsHistory_poker <- xml_data_frame("data/poker.stackexchange.com/PostHistory.xml")
Posts_poker <- xml_data_frame("data/poker.stackexchange.com/Posts.xml")
Users_poker <- xml_data_frame("data/poker.stackexchange.com/Users.xml")
Tags_poker <- xml_data_frame("data/poker.stackexchange.com/Tags.xml")
Votes_poker <- xml_data_frame("data/poker.stackexchange.com/Votes.xml")

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
  lokalizacje <- matrix(unlist(lapply(location,function(x) geocode(x[1],source = "google"))),ncol = 2,byrow = TRUE)
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

save_locations(Users_beer)
locations_beers <- readRDS("Users_beer_location.rds")

par(mar=c(0,0,0,0))
map('world',col="#f2f2f2", fill=TRUE, bg="white", lwd=0.05,mar=rep(0,4),border=0, ylim=c(-80,80),main ="mapa")
points(lokalizacje$lon,lokalizacje$lan,pch = 16,cex = 0.7, col = rgb(red = 0, green = 0, blue = 0, alpha = 0.2))

