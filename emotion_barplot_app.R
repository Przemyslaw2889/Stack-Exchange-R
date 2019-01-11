library(ggplot2)


######KOMENTARZE
#GAMING
df_sentiment_gaming_comm <- readRDS("gaming_df_sentiment.rds")

df_sentiment_gaming_comm$value[df_sentiment_gaming_comm$variable == "liczba"] <- df_sentiment_gaming_comm$value[
  df_sentiment_gaming_comm$variable == "liczba"]/sum(df_sentiment_gaming_comm$value[df_sentiment_gaming_comm$variable == "liczba"])

df_sentiment_gaming_comm <- df_sentiment_gaming_comm[11:20,]
df_sentiment_gaming_comm$variable <- c("gaming")

#BEER

df_sentiment_beer_comm <- readRDS("df_sentiment_beer.rds")
df_sentiment_beer_comm$variable <- c("beer")


#COFFEE
df_sentiment_coffee_comm <- readRDS("df_sentiment_coffee_comm.rds")
df_sentiment_coffee_comm <- df_sentiment_coffee_comm[11:20,]
df_sentiment_coffee_comm$variable <- "coffee"


Comments <-rbind(df_sentiment_coffee_comm,df_sentiment_beer_comm,df_sentiment_gaming_comm)




#####POSTY
#GAMING
df_sentiment_gaming_post <- readRDS("df_sentiment_gaming_post.rds")
df_sentiment_gaming_post <- df_sentiment_gaming_post[11:20,]
df_sentiment_gaming_post$variable <- "gaming"
df_sentiment_gaming_post$value <- df_sentiment_gaming_post$value/sum(df_sentiment_gaming_post$value)

#BEER
df_sentiment_beer_post <- readRDS("df_sentiment_beer_post.rds")


#COFFEE
df_sentiment_coffee_post <- readRDS("df_sentiment_coffee_post.rds")

Posts <- rbind(df_sentiment_gaming_post,df_sentiment_beer_post,df_sentiment_coffee_post)

barplot_emotion <- function(typ, emocje = unique(Comments$emocja))
{
  ggplot(typ[typ$emocja %in% emocje, ],aes(x = reorder(emocja,-value), y = value, fill = variable)) +
    geom_bar(stat="identity",position='dodge')+
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("emocje") +ylab("% udzia³") + labs(title = "Rozk³ad emocji")
  
}

#barplot_emotion(Comments, c("positive",'negative'))


