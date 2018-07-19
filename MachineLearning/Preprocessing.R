library(tm)
library(SnowballC)

text_preprocessing <- function(df) {
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
  colnames(dtmdf) = paste("word_", colnames(dtmdf), sep = "")
  
  df = cbind(df, dtmdf)
  df
}