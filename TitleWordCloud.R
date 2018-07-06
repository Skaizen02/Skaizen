library(tm)
library(RColorBrewer)
library(wordcloud)

#Create corpus
myCorpus <- Corpus(VectorSource(unique(df$title)))
myCorpus <- tm_map(myCorpus, content_transformer(tolower))

#Remove numbers and punctuation
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct))

#Remove stop words
myStopwords <- c(stopwords("english"), "available", "via")
myStopwords <- setdiff(myStopwords, c("r", "big"))
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
myCorpus <- tm_map(myCorpus, stripWhitespace)

#Stemming corpus
myCorpusCopy <- myCorpus
myCorpus <- tm_map(myCorpus, stemDocument)

stemCompletion2 <- function(x, dictionary) {
  
  x <- unlist(strsplit(as.character(x), " "))
  x <- x[x != ""]
  x <- stemCompletion(x, dictionary=dictionary)
  x <- paste(x, sep="", collapse=" ")
  PlainTextDocument(stripWhitespace(x))
}

#Stem completion
myCorpus <- lapply(myCorpus, stemCompletion2, dictionary=myCorpusCopy)
myCorpus <- lapply(myCorpus, as.character)
myCorpus <- Corpus(VectorSource(myCorpus))

#Manual word replacement
replaceWord <- function(corpus, oldword, newword) {
  tm_map(corpus, content_transformer(gsub), pattern=oldword, replacement=newword)
}


tdm <- TermDocumentMatrix(myCorpus, control=list(wordLengths=c(1,Inf)))

#Identifying reccuring terms
m <- as.matrix(tdm)
termFrequency <- rowSums(as.matrix(tdm)) #Calculating frequencies
termFrequency <- subset(termFrequency, termFrequency >= 10) #Filtering
freq.df <- data.frame(term=names(termFrequency), freq=termFrequency) #To data frame
word.freq <- sort(rowSums(m), decreasing = TRUE) #Sorting
pal <- brewer.pal(9, "BuGn")[-(1:4)] #Creating color palette

wordcloud(words = names(word.freq), freq = word.freq, min.freq = 3, random.order = F, colors = pal)