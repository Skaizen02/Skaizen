require(devtools)
install_github("okugami79/sentiment140", force = TRUE)
library(topicmodels)
library(sentiment)

sentiments <- sentiment(df$title)
table(sentiments$polarity)

sentiments$score <- 0
sentiments$score[sentiments$polarity == "positive"] <- 1
sentiments$score[sentiments$polarity == "negative"] <- -1
sentiments$date <- as.character(as.POSIXlt(df$time, origin = "1970-01-01"))
result <- aggregate(score ~ date, data = sentiments, sum)
plot(result, type = "l")

dtm <- as.DocumentTermMatrix(tdm)

lda <- LDA(dtm, k = 8)
term <- terms(lda, 7)
(term <- apply(term, MARGIN = 2, paste, collapse = ", "))

topics <- topics(lda)
topics <- data.frame(date=as.Date(df$created), topic=topics)
ggplot(topics, aes(date, fill = term[topic])) + geom_density(position = "stack")