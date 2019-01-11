options(stringsAsFactors = FALSE)
library(dplyr)

Tags_beer <- read.csv("data/beer.stackexchange.com/Tags.csv")
Tags_coffee <- read.csv("data/coffee.stackexchange.com/Tags.csv")
Tags_gaming <- read.csv("data/gaming.stackexchange.com/Tags.csv")

# Funkcja tworzy barplot k najczesciej wystepujacych tagow
tags_barplot <- function(Tags, k=10, ...){
  # Przejrzymy najpierw jakie sa popularne tagi
  popular_tags <- Tags%>% 
    arrange(desc(Count)) %>% head(k)
  #print(popular_tags)
  # Barplot tagow
  counts <- table(popular_tags$Count)
  #print(counts)
  #opt <- par(mai=c(1,2.5,1,1))
  #barplot(popular_tags$Count, names.arg=popular_tags$TagName, las=2, horiz=TRUE, ...)
  ggplot(popular_tags,aes(x = reorder(TagName,Count), y = Count))+ geom_bar(stat="identity",position='dodge') +
     coord_flip() + xlab("TagName") + labs(title = "Popular Tags")
  }

# tags_barplot(Tags_beer)
# tags_barplot(Tags_gaming, 10)
# tags_barplot(Tags_coffee, 5)

# tags_barplot_name <- function(name, k=10, ...){
#   tags = list(coffee=Tags_coffee, beer=Tags_beer, gaming=Tags_gaming)
#   tags_barplot(tags[[name]], k, ...)
# }

