now <- as.integer(Sys.time())
df$popularity <- unlist(list(0))

df$popularity <- df$views / (now - df$time)

df <- df[order(df$id),]

tags <- c()
for (i in df$id) {
  tags <- unique(c(tags, unlist(strsplit(df$tags[i], ","))))
}

tags <- data.frame(tag = tags)
tags$popularity <- unlist(list(0))
for (i in seq.int(length(tags$tag))) {
  currentTag <- tags$tag[i]
  for (j in df$id) {
    postTags <- unlist(strsplit(df$tags[j], ","))
    if (currentTag %in% postTags) {
      tags$popularity[i] <- tags$popularity[i] + df$popularity[j]
    }
  }
}

#Calculating total popularity
tags$popularity <- list(0)
for (i in seq.int(length(tags))) {
  for (j in df$id) {
    if (tags$tag[i] %in% strsplit(df$postTags[j])) {
      tags$popularity[i] <- tags$popularity[i] + df$popularity[j]
    }
  }
}

tags$occurences <- table(unlist(strsplit(df$tags, ",")))
