#' Parser Function
#' 
#' This function gathers data out of the given pages.
#' @param links A list of links to the pages.
#' @param limit The number of pages you want to parse. 0 means all. Defaults to 0.
#' @param beep Will make a sound when finished if set to TRUE. Defaults to FALSE.
#' @export
#' @examples
#' df <- parser(links, 100, T)


library(rvest)
library(tm)
library(beepr)
library(progress)

parser <- function(links, limit = 0, step = 1, beep = T) {
  selector <-  ".left.fullWidth:not(.left.fullWidth.upper.fontColorOne),
                #ctl00_ctl00_ConMainBody_ConMainBody_ctl01_pnlBody,
                .strong.fullWidth,
                .ncMetaDataSnippet,
                #ctl00_ctl00_ConMainBody_ConMainBody_ctl01_lblInfo,
                #twitterResult,
                #liResult,
                #fbResult,
                #reResult,
                #goResult,
                #emResult"
  
  months <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
  
  startTime <- as.integer(Sys.time())
  df <- data.frame(url = unlist(links))

  df$social = NA
  df$title  = NA
  df$postTags = NA
  df$time = NA
  df$text = NA
  
  if (limit == 0) {
    l <- length(links)
  } else {
    l <- limit
  }
  
  pb <- progress_bar$new(total = l, format = ":spin [:bar] :percent(:i/:total : :tick_rate) : :elapsed/:eta")
  
  for(i in seq.int(l)) {
    link <- links[[i]]  
    html <- read_html(link)
    
    data <- html_text(html_nodes(html, selector))
    
    #Social networks
    df$social[i] <- sum(as.numeric(data[1:6]))
    
    #Title
    df$title[i] <- data[7]
    title_time <- as.numeric(Sys.time())
    
    #Text
    df$text[i] <- paste(data[9], data[10])
    text_time <- as.numeric(Sys.time())
    
    #Tags
    if (length(data) > 10) {
      df$postTags[i] <- paste(data[11:length(data)], collapse = ",")
    }
    
    #Stats
    stats <- unlist(strsplit(data[8], "[[:space:]]"))
    
    #Views
    df$views[i] <- as.integer(stats[7])
    
    #Comments
    df$comments[i] <- as.integer(stats[12])
    
    #Time
    if (stats[2] == "hours" || stats[2] == "hour") {
      hours <- as.integer(stats[1])
      df$time[i] <- startTime - hours * 3600
    } else if (stats[2] == "minutes" || stats[2] == "minute") {
      minutes <- as.integer(stats[1])
      df$time[i] <- startTime - minutes * 60
    } else {
      year <- as.integer(stats[3])
      month <- which(months == stats[2])
      day <- as.integer(stats[1])
      date <- ISOdate(year, month, day)
      df$time[i] <- as.integer(date)
    }

    pb$tick(tokens = list(i = as.character(i)))
  }
  df <- subset(df, !is.na(title))
  df$id <- seq.int(length(df$url))
  if (beep) {
    beep() 
  }
  df[1:length(df$title),]
}

parser <- function(url) {
  startTime <- as.numeric(Sys.time())
  html <- read_html(url)
  closeAllConnections()
  
  nodes <- html_nodes(html, "a")
  pageLinks <- html_attr(nodes, 'href')

  data <- html_text(html_nodes(html, selector))
  
  #Social networks
  social <- sum(as.numeric(data[1:6]))
  
  #Title
  title <- data[7]
  
  #Text
  text <- paste(data[9], data[10])
  
  #Tags
  if (length(data) > 10) {
    postTags <- paste(data[11:length(data)], collapse = ",")
  }
  
  #Stats
  stats <- unlist(strsplit(data[8], "[[:space:]]"))
  
  #Views
  views <- as.integer(stats[7])
  
  #Comments
  comments <- as.integer(stats[12])
  
  #Time
  if (stats[2] == "hours" || stats[2] == "hour") {
    hours <- as.integer(stats[1])
    time <- startTime - hours * 3600
  } else if (stats[2] == "minutes" || stats[2] == "minute") {
    minutes <- as.integer(stats[1])
    time <- startTime - minutes * 60
  } else {
    year <- as.integer(stats[3])
    month <- which(months == stats[2])
    day <- as.integer(stats[1])
    date <- ISOdate(year, month, day)
    time <- as.integer(date)
  }
  
  list(links = pageLinks, data = list(title = title, text = text, social = social, views = views, comments = comments, time = time))
}