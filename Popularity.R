now <- as.integer(Sys.time())

df$popularity <- df$views / (now - df$time) * 1000

df <- df[order(df$id),]

tags <- c()
for (i in df$id) {
  tags <- unique(c(tags, unlist(strsplit(df$postTags[i], ","))))
}
tags <- data.frame(tag = tags)

tags$popularity <- unlist(list(0))
for (i in seq.int(length(tags$tag))) {
  currentTag <- tags$tag[i]
  for (j in df$id) {
    postTags <- unlist(strsplit(df$postTags[j], ","))
    if (currentTag %in% postTags) {
      tags$popularity[i] <- tags$popularity[i] + df$popularity[j]
    }
  }
}

#Calculating total popularity
tags$total_pop <- unlist(list(0))
for (i in seq.int(length(tags))) {
  for (j in df$id) {
    if (tags$tag[i] %in% strsplit(df$postTags[j], ",")) {
      tags$total_pop[i] <- tags$total_pop[i] + df$popularity[j]
    }
  }
}

df <- df[order(df$id),]
tags$total_pop <- unlist(list(0))
for (i in df$id) {
  for (postTag in strsplit(df$postTags[i], ",")) {
    index <- which(tags$tag == postTag)
    tags$total_pop[index] <- tags$total_pop[index] + df$popularity[i]
  }
}

tags$avg_pop <- tags$total_pop/tags$occurences
tags$occurences <- table(unlist(strsplit(df$postTags, ",")))
