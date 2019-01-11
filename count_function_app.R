#COFFEE
# Votes_coffee <- read.csv("data/coffee.stackexchange.com/Votes.csv")
Comments_coffee <- read.csv("data/coffee.stackexchange.com/Comments.csv")
Posts_coffee <- read.csv("data/coffee.stackexchange.com/Posts.csv")


# Votes_coffee <- data.frame(date = stri_datetime_format(as.Date(Votes_coffee$CreationDate),"yyyy-MM")) %>% group_by(date) %>%
#   summarise(liczba = n())
# Votes_coffee$typ <- "Votes"
Posts_coffee <- data.frame(date = stri_datetime_format(as.Date(Posts_coffee$CreationDate),"yyyy-MM")) %>% group_by(date) %>%
  summarise(liczba = n())
Posts_coffee$typ <- "Posts"
Comments_coffee <- data.frame(date = stri_datetime_format(as.Date(Comments_coffee$CreationDate),"yyyy-MM")) %>% group_by(date) %>%
  summarise(liczba = n())
Comments_coffee$typ <- "Comments"


coffee_df <- rbind(Posts_coffee,Comments_coffee)


#BEER
# Votes_beer <- read.csv("data/beer.stackexchange.com/Votes.csv")
Comments_beer <- read.csv("data/beer.stackexchange.com/Comments.csv")
Posts_beer <- read.csv("data/beer.stackexchange.com/Posts.csv")


# Votes_beer <- data.frame(date = stri_datetime_format(as.Date(Votes_beer$CreationDate),"yyyy-MM")) %>% group_by(date) %>%
#   summarise(liczba = n()) %>% arrange(date)
# Votes_beer$typ <- "Votes"
Posts_beer <- data.frame(date = stri_datetime_format(as.Date(Posts_beer$CreationDate),"yyyy-MM")) %>% group_by(date) %>%
  summarise(liczba = n()) %>% arrange(date)
Posts_beer$typ <- "Posts"
Comments_beer <- data.frame(date = stri_datetime_format(as.Date(Comments_beer$CreationDate),"yyyy-MM")) %>% group_by(date) %>%
  summarise(liczba = n()) %>% arrange(date)
Comments_beer$typ <- "Comments"


beer_df <- rbind(Posts_beer,Comments_beer)



#GAMING
# Votes_gaming <- read.csv("data/gaming.stackexchange.com/Votes.csv")
Comments_gaming <- read.csv("data/gaming.stackexchange.com/Comments.csv")
Posts_gaming <- read.csv("data/gaming.stackexchange.com/Posts.csv")


# Votes_gaming <- data.frame(date = stri_datetime_format(as.Date(Votes_gaming$CreationDate),"yyyy-MM")) %>% group_by(date) %>%
#   summarise(liczba = n()) %>% arrange(date)
# Votes_gaming$typ <- "Votes"
Posts_gaming <- data.frame(date = stri_datetime_format(as.Date(Posts_gaming$CreationDate),"yyyy-MM")) %>% group_by(date) %>%
  summarise(liczba = n()) %>% arrange(date)
Posts_gaming$typ <- "Posts"
Comments_gaming <- data.frame(date = stri_datetime_format(as.Date(Comments_gaming$CreationDate),"yyyy-MM")) %>% group_by(date) %>%
  summarise(liczba = n()) %>% arrange(date)
Comments_gaming$typ <- "Comments"


gaming_df <- rbind(Posts_gaming,Comments_gaming)

list_of_count <- list(gaming = gaming_df, beer = beer_df, coffee = coffee_df)

count_post_comment <- function(forum,typ){
  if (forum == "gaming"){
  ggplot(list_of_count[[forum]], aes(x = date,y = liczba,fill = typ)) + geom_bar(stat="identity",position='dodge') +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  scale_x_discrete(breaks = unique(gaming_all$date)[
      seq(1, length(unique(gaming_all$date)), by = 3)]) + labs(title = paste("Number of posts and comments on",forum,"forum"))
    }
  else{
    ggplot(list_of_count[[forum]], aes(x = date,y = liczba,fill = typ)) + geom_bar(stat="identity",position='dodge') +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  scale_x_discrete(breaks = unique(gaming_all$date)[
        seq(1, length(unique(gaming_all$date)), by = 2)]) + labs(title = paste("Number of posts and comments on",forum,"forum"))
  }
}

#count_post_comment("beer")
