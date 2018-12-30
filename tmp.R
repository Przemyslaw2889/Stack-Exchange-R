source("df_from_xml.R")
options(stringsAsFactors = FALSE)
library(ggmap)
library(stringi)
library(maps)

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
locations_coffee <- readRDS("Users_beer_location.rds")

save_locations(Users_beer)
locations_beers <- readRDS("Users_beer_location.rds")

save_locations(Users_gaming)
locations_beers <- readRDS("Users_beer_location.rds")

par(mar=c(0,0,0,0))
map('world',col="#f2f2f2", fill=TRUE, bg="white", lwd=0.05,mar=rep(0,4),border=0, ylim=c(-80,80),main ="mapa")
points(lokalizacje$lon,lokalizacje$lan,pch = 16,cex = 0.7, col = rgb(red = 0, green = 0, blue = 0, alpha = 0.2))

