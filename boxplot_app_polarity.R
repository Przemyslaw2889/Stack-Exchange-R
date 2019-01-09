library(ggplot2)

polarity_gaming <- readRDS("polarity_gaming.rds")
polarity_coffee <- readRDS("polarity_coffee.rds")
polarity_beer <- readRDS("polarity_beer.rds")


comment_ID <- data.frame(polarity = c(polarity_gaming$polarity[polarity_gaming$by == "comment_ID"],
                                              polarity_beer$polarity[polarity_beer$by == "comment_ID"],
                                              polarity_coffee$polarity[polarity_coffee$by == "comment_ID"]),
                                  forum = c(rep("gaming",length(polarity_gaming$polarity[polarity_gaming$by == "comment_ID"])),
                                            rep("beer",length(polarity_beer$polarity[polarity_beer$by == "comment_ID"])),
                                            rep("coffe",length(polarity_coffee$polarity[polarity_coffee$by == "comment_ID"]))))


# ggplot(polarity_comment_ID,aes(y = polarity, x = forum, color = forum)) + geom_boxplot(outlier.colour="black", outlier.shape=16,
#                                                                              outlier.size=2,outlier.alpha = 0.1)+
#   labs(title = "porownanie Comment_ID")



post <- data.frame(polarity = c(polarity_gaming$polarity[polarity_gaming$by == "post"],
                                               polarity_beer$polarity[polarity_beer$by == "post"],
                                               polarity_coffee$polarity[polarity_coffee$by == "post"]),
                                  forum = c(rep("gaming",length(polarity_gaming$polarity[polarity_gaming$by == "post"])),
                                            rep("beer",length(polarity_beer$polarity[polarity_beer$by == "post"])),
                                            rep("coffe",length(polarity_coffee$polarity[polarity_coffee$by == "post"]))))

# 
# ggplot(polarity_post,aes(y = polarity, x = forum, color = forum)) + geom_boxplot(outlier.colour="black", outlier.shape=16,
#                                                                                        outlier.size=2,outlier.alpha = 0.1)+
#   labs(title = "porownanie post")




user <- data.frame(polarity = c(polarity_gaming$polarity[polarity_gaming$by == "user"],
                                         polarity_beer$polarity[polarity_beer$by == "user"],
                                         polarity_coffee$polarity[polarity_coffee$by == "user"]),
                            forum = c(rep("gaming",length(polarity_gaming$polarity[polarity_gaming$by == "user"])),
                                      rep("beer",length(polarity_beer$polarity[polarity_beer$by == "user"])),
                                      rep("coffe",length(polarity_coffee$polarity[polarity_coffee$by == "user"]))))


# ggplot(polarity_user,aes(y = polarity, x = forum, color = forum)) + geom_boxplot(outlier.colour="black", outlier.shape=16,
#                                                                                  outlier.size=2,outlier.alpha = 0.1)+
#   labs(title = "porownanie user")

option_list = list(comments = comment_ID, posts = post)

boxplot_app <- function(by){
  ggplot(option_list[[by]], aes(y = polarity, x = forum, color = forum)) + geom_boxplot(outlier.colour="black", outlier.shape=16,
                                                                                   outlier.size=2,outlier.alpha = 0.1)+
    labs(title = "Comparison of polarity between forums")
}

