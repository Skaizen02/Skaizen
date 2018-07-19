library(tm)
library(SnowballC)
library(dplyr)
library(keras)

preprocessing <- function(df) {
  df <- select(df, c("views", "text", "time", "postTags"))
  
  #Identifying keywords
  docs = Corpus(VectorSource(df$text))
  
  docs = tm_map(docs, removePunctuation)
  docs = tm_map(docs, removeNumbers)
  docs = tm_map(docs, tolower)
  docs = tm_map(docs, removeWords, stopwords("english"))
  docs = tm_map(docs, stripWhitespace)
  docs = tm_map(docs, stemDocument)
  
  dtm = DocumentTermMatrix(docs)
  
  dtmdf = data.frame(as.matrix(dtm))
  freq = colSums(as.matrix(dtm))
  dtmdf = dtmdf[,order(freq, decreasing = T)[1:128]]
  dtmdf[] = lapply(dtmdf, function(x) ifelse(x>0, 1, 0))
  colnames(dtmdf) = paste("word", colnames(dtmdf), sep = "_")
  
  df = cbind(df, dtmdf)
  
  #Identifying tags
  tagList <- unique(unlist(strsplit(df$postTags, ",")))
  tagdf <- NULL
  tagdf <- data.frame(lapply(tagList, function(tag){
    unlist(lapply(df$postTags, function(postTags){
      ifelse(tag %in% unlist(strsplit(postTags, ",")), 1, 0)
    }))
  }))
  colnames(tagdf) <- paste("tag", tagList, sep = "_")
  df <- cbind(df, tagdf)
  
  #Identifying week days
  wdays = as.POSIXlt(df$time, origin = "1970-01-01")$wday
  
  wdaydf <- data.frame(to_categorical(wdays, 7))
  colnames(wdaydf) <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
  
  df <- cbind(df, wdaydf)
  select(df, -c("text", "time", "postTags"))
}