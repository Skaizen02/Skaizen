boolTags = function(df) {
  tagData = unlist(strsplit(df$postTags, ","))
  tagData = data.frame(table(tagData))
  tagData = tagData[order(tagData$Freq, decreasing = T),]
  tagList = tagData$tagData[1:15]
  message(sprintf("Acquired tag list of length %s.", length(tagList)))
  
  for (i in seq.int(length(tagList))) {
    tag = tagList[i]
    message(sprintf("%s / %s", i, length(tagList)))
    column = list()
    for (j in df$id) {
      column[j] = tag %in% strsplit(df$postTags[j], ",")[[1]]
    }
    column = data.frame(unlist(column))
    colnames(column) = tag
    df = cbind(df, column)
  }
  
  df
}