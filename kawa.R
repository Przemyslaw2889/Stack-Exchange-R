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

library(ggmap)
library(stringi)
location <- strsplit(Users$Location[stri_detect_regex(Users$Location,",")], split = ",")
location <- location[!is.na(location)]
lokalizacje <- matrix(unlist(lapply(location[c(1:10)],function(x) geocode(x[1],source = "dsk"))),ncol = 2,byrow = TRUE)
lokalizacje <- as.data.frame(lokalizacje)
colnames(lokalizacje) <- c("lon","lan")
