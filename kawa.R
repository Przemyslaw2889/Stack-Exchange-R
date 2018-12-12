library(XML)
options(stringsAsFactors = FALSE)
setwd("coffee.stackexchange.com/")
changes_names <- function(x,nazwy){
  names(x) <- nazwy
  x
}

xml_data_frame <- function(xml){
  df <- xmlToList(xml)
  len <- lapply(df,length)
  n <- which(unlist(len) == max(unlist(len)))[1]          
  nazwy <- names(df[[n]])
  df <- lapply(df,"[",nazwy)
  df <- lapply(df, changes_names,nazwy)
  df <- data.frame(t(data.frame(df,check.names = FALSE)))
  rownames(df) <- 1:nrow(df)
  df
}
Comments <- xml_data_frame("Comments.xml")
Badges <- xml_data_frame("Badges.xml")
PostsHistory <- xml_data_frame("PostHistory.xml")
Posts <- xml_data_frame("Posts.xml")
Users <- xml_data_frame("Users.xml")
Tags <- xml_data_frame("Tags.xml")
Votes <- xml_data_frame("Votes.xml")

library(ggmap)
library(stringi)
location <- strsplit(Users$Location[stri_detect_regex(Users$Location,",")], split = ",")
location <- location[!is.na(location)]
lokalizacje <- matrix(unlist(lapply(location[c(1:10)],function(x) geocode(x[1],source = "dsk"))),ncol = 2,byrow = TRUE)
lokalizacje <- as.data.frame(lokalizacje)
colnames(lokalizacje) <- c("lon","lan")
