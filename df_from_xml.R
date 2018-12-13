library(XML)
options(stringsAsFactors = FALSE)

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
