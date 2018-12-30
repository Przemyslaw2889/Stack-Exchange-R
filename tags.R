options(stringsAsFactors = FALSE)
library(dplyr)

Tags_beer <- read.csv("data/beer.stackexchange.com/Tags.csv")
Tags_coffee <- read.csv("data/coffee.stackexchange.com/Tags.csv")
Tags_gaming <- read.csv("data/gaming.stackexchange.com/Tags.csv")

tags_barplot <- function(Tags, k=10, ...){
  # Przejrzymy najpierw jakie sa popularne tagi
  popular_tags <- Tags%>% 
    arrange(desc(Count)) %>% head(k)
  
  # Barplot tagow
  counts <- table(popular_tags$Count)
  opt <- par(mai=c(1,2.5,1,1))
  barplot(popular_tags$Count, names.arg=popular_tags$TagName, las=2, horiz=TRUE, ...)
}

tags_barplot(Tags_beer)
tags_barplot(Tags_gaming, 10)
tags_barplot(Tags_coffee, 5)
