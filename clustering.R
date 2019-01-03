options(stringsAsFactors = FALSE)
Posts_coffee <- read.csv("data/coffee.stackexchange.com/Posts.csv")
Posts_coffee$Tags <- stri_extract_all_regex(Posts_coffee$Tags,"[a-z]+\\-[a-z]+|[a-z]+")
#biblioteki
library(tm)
library(purrr)
library(ggplot2)
library(factoextra)
library(stringi)
library(RColorBrewer)
clean_corpus <- function(corpus) {
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, c(stopwords("en"),"coffee"))
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}
#######COFFEE
#opracowanie macierzy wystapien
Posts_coffee_corp <- clean_corpus(VCorpus(VectorSource(Posts_coffee$Body)))
dtm_post <- as.matrix(DocumentTermMatrix(Posts_coffee_corp))
sum_col <- apply(dtm_post,2,sum)
sum_col <- sort(sum_col,decreasing = TRUE)
top_10_word <- names(sum_col[1:15])
#wezmy 3000 najczesciej wystepujacych slow
dtm_post <- dtm_post[,names(sum_col[1:3000])]
#wybor k
tot_withinss <- map_dbl(1:10,  function(k){
  model <- kmeans(x = dtm_post, centers = k)
  model$tot.withinss
})

elbow_df <- data.frame(
  k = 1:10 ,
  tot_withinss = tot_withinss
)

ggplot(elbow_df, aes(x = k, y = tot_withinss)) +
  geom_point() +
  scale_x_continuous(breaks = 1:10)

#wezmy k = 2

means <- kmeans(dtm_post, centers = 2)
top_10_word_df <- data.frame(dtm_post)
top_10_word_df$cluster <- means$cluster
mean_top10 <- top_10_word_df %>% group_by(cluster) %>% summarise_all(funs(mean(.))) %>% as.matrix()

mean_top10 <- data.frame(mean = c(mean_top10[1,2:3001],mean_top10[2,2:3001]), cluster = c(rep("cluster1",3000),rep("cluster2",3000)),
                         word = rep(colnames(mean_top10)[2:3001],2))

mean_cluster1  <- mean_top10 %>% filter(cluster == "cluster1") %>% top_n(n = 10,wt = mean)

mean_cluster2  <- mean_top10 %>% filter(cluster == "cluster2") %>% top_n(n = 10,wt = mean)

top_word <- mean_top10 %>% filter(word %in% unique(c(mean_cluster1$word, mean_cluster2$word)))


#wykresy
ggplot(top_word, aes(y = mean, x = reorder(word,-mean), fill = cluster)) + geom_bar(stat="identity",position=position_dodge()) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_brewer(type = "seq",palette = "Set2")
