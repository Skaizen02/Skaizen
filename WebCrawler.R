library(Rcrawler)
library(rvest)
library(tm)

url <- "https://www.finextra.com/"

#Add titles to the database
#addTitles()

addTitles <- function() {
  df <- data.frame(matrix(unlist(DATA)))
  df <- na.omit(df)
  names(df)[1] = "titles"
  df <- data.frame(titles = unique(df$titles))
  db <- dbReadTable(con, "webcrawler")
  df$tags <- list(NULL)
  df$tags[0:length(db$tags)] <- db$tags
  df$id <- seq.int(length(df))
  dbWriteTable(conn=con, name="webcrawler", value=df, row.names = FALSE, overwrite = TRUE)
}

#Add tags to the data base
#addTags()

addTags <- function() {
  df <- data.frame(matrix(unlist(DATA)))
  names(df)[1] = "tags"
  df <- data.frame(titles = unique(df$titles))
  db <- dbReadTable(con, "webcrawler")
  df$tags <- list(NULL)
  df$tags[0:length(db$tags)] <- db$tags
  df$id <- seq.int(length(df$tags))
  dbWriteTable(conn=con, name="webcrawler", value=df, row.names = FALSE, overwrite = TRUE)
}

#Find links
# links <- c()
# findLinks(url, 2)


#Crawler
titleSelector <- ".left.fullWidth:not(.left.fullWidth.upper.fontColorOne)"
tagSelector <- ".ncMetaDataSnippet"
statsSelector <- "#ctl00_ctl00_ConMainBody_ConMainBody_ctl01_lblInfo"

crawler <- function(iterations, url, path) {
  links <- list(url)
  scannedLinks <- c()
  for (i in 0:iterations) {
    tmp <- links
    for (j in seq.int(length(links))) {
      link <- links[j][[1]]
      if (!(link %in% scannedLinks)) {
        pageLinks <- LinkExtractor(link)[[2]]
        pageLinks <- c(lapply(pageLinks, function (x) {
          if (startsWith(x, path)) {
            x
          }
        }))
        pageLinks <- pageLinks[!sapply(pageLinks, is.null)]
        tmp <- c(tmp, pageLinks)
        scannedLinks <- c(scannedLinks, link)
      }
    }
    links <- unique(tmp)
  }
  if (!startsWith(url, path)) {
    links[1] <- NULL
  }
  links <- lapply(links, function(x) as.character(x))
  links
}

parser <- function(links, limit = 0, step = 1) {
  df <- data.frame(url = unlist(links))
  df$title <- unlist(list(NA))
  df$postTags <- unlist(list(NA))
  df$views <- unlist(list(NA))
  df$comments <- unlist(list(NA))
  df$time <- unlist(list(NA))
  if (limit == 0) {
    l <- length(links)
  } else {
    l <- limit
  }
  
  time <- Sys.time()
  
  for(i in seq.int(l)) {
    if (i %% step == 0) {
      progress_bar <- paste(c("[", lapply(seq.int(10), function(x, progress) {
        if (x <= progress) {
          "#"
        } else {
          " "
        }
      }, round(i/l*10)), "] "), sep = "", collapse = "")
      stepindicator <- sprintf("%s/%s", i, l, sep = ",")
      print(paste(c( progress_bar, stepindicator), sep = "", collapse = ""))
    }
    link <- links[[i]]  
    html <- read_html(link)
    
    #Title
    title_node <- html_node(html, titleSelector)
    df$title[i] <- html_text(title_node)
    
    #Tags
    tags_nodes <- html_nodes(html, tagSelector)
    if (length(tags_nodes) > 0) {
      tags <- paste(lapply(tags_nodes, function(x) html_text(x)), collapse = ",")
      df$postTags[i] <- tags 
    }
    
    stats_node <- html_node(html, statsSelector)
    text <- html_text(stats_node)
    text <- stripWhitespace(text)
    splitText <- unlist(strsplit(text, "[[:space:]]"))
    len <- length(splitText)
    df$text[i] <- text
    
    #Views
    df$views[i] <- as.integer(splitText[len-3])
    
    #Comments
    df$comments[i] <- as.integer(splitText[len])
    
    #Time
    timestamp <- as.integer(Sys.time())
    if (splitText[2] == "hours") {
      hours <- as.integer(splitText[1])
      postTime <- time - hours * 3600
      df$time[i] <- as.integer(postTime)
    } else {
      year <- splitText[3]
      month <- splitText[2]
      day <- splitText[1]
      date <- ISOdate(year, month, day)
      print(date)
      df$time[i] <- as.integer(date)
    }
  }
  df$tags <- unlist(df$tags)
  df$url <- unlist(df$url)
  df$title <- unlist(df$title)
  df$views <- unlist(df$views)
  df$comments <- unlist(df$comments)
  df$time <- unlist(df$time)
  df <- subset(df, !is.na(title))
  df$id <- seq.int(length(df$url))
  df
}
