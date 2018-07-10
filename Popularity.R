now <- as.integer(Sys.time())

df$popularity <- round(df$views / (now - df$time) * 1000, 2)

df <- df[order(df$id),]
tags <- c()
for (i in df$id) {
  tags <- unique(c(tags, unlist(strsplit(df$postTags[i], ","))))
}
tags <- data.frame(tag = tags)

df <- df[order(df$id),]
tags$total_pop <- unlist(list(0))
for (i in df$id) {
  for (postTag in unlist(strsplit(df$postTags[i], ","))) {
    index <- which(tags$tag == postTag)
    tags$total_pop[index] <- tags$total_pop[index] + df$popularity[i]
  }
}

tags <- tags[order(tags$tag),]
tags$occurences <- table(unlist(strsplit(df$postTags, ",")))
tags$avg_pop <- round(tags$total_pop/tags$occurences, 2)