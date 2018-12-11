library(XML)
options(stringsAsFactors = FALSE)
#³adanowanie danych pe³nych
Badges <- data.frame(t(data.frame(xmlToList("Badges.xml"))),row.names = 1:8031)
Comments <- data.frame(t(data.frame(xmlToList("Comments.xml"))),row.names = 1:3593)

#³adowanie danych z brakiem niektorych zmiennych (uzupe³nienie ich NA)

changes_names <- function(x,nazwy){
  names(x) <- nazwy
  x
}

PostHistory <- xmlToList("PostHistory.xml")
nazwy <- names(PostHistory[[19]])
PostHistory <- lapply(PostHistory,"[",nazwy)
PostHistory <- lapply(PostHistory, changes_names,nazwy)
PostHistory <- data.frame(t(data.frame(PostHistory,check.names = FALSE)))
rownames(PostHistory) <- 1:nrow(PostHistory)

Posts <- xmlToList("Posts.xml")
len <- lapply(Posts,length)
n <- which(unlist(len) == max(unlist(len)))[1]          
nazwy <- names(Posts[[n]])
Posts <- lapply(Posts,"[",nazwy)
Posts <- lapply(Posts, changes_names,nazwy)
Posts <- data.frame(t(data.frame(Posts,check.names = FALSE)))
rownames(Posts) <- 1:nrow(Posts)


Users <- xmlToList("Users.xml")
len <- lapply(Users,length)
n <- which(unlist(len) == max(unlist(len)))[1]          
nazwy <- names(Users[[n]])
Users <- lapply(Users,"[",nazwy)
Users <- lapply(Users, changes_names,nazwy)
Users <- data.frame(t(data.frame(Users,check.names = FALSE)))
rownames(Users) <- 1:nrow(Users)


Tags <- xmlToList("Tags.xml")
len <- lapply(Tags,length)
n <- which(unlist(len) == max(unlist(len)))[1]          
nazwy <- names(Tags[[n]])
Tags <- lapply(Tags,"[",nazwy)
Tags <- lapply(Tags, changes_names,nazwy)
Tags <- data.frame(t(data.frame(Tags,check.names = FALSE)))
rownames(Tags) <- 1:nrow(Tags)


Votes <- xmlToList("Votes.xml")
len <- lapply(Votes,length)
n <- which(unlist(len) == max(unlist(len)))[1]          
nazwy <- names(Votes[[n]])
Votes <- lapply(Votes,"[",nazwy)
Votes <- lapply(Votes, changes_names,nazwy)
Votes <- data.frame(t(data.frame(Votes,check.names = FALSE)))
rownames(Votes) <- 1:nrow(Votes)
