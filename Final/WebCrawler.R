library(Rcrawler)
library(rvest)
library(tm)
library(beepr)
library(progress)

url = "https://www.finextra.com"
path = "/newsarticle"

#Link Extraction
extractor = function(page) {
  pageLinks = ""
  tryCatch({
    html = read_html(url(page, "rb"))
    nodes = html_nodes(html, "a")
    pageLinks = html_attr(nodes, 'href')
  }, error = function(err) {
    print(err)
    print(page)
  })
  closeAllConnections()
  return(pageLinks)
}

#Crawler
crawler <- function(iterations, url, path, step = 10, n = 100, beep = F) {
  links <- list(url)
  pb <- progress_bar$new(total = n, format = "[:bar] :percent : :elapsed/:eta")
  scannedLinks <- c()
  for (i in 0:iterations) {
    tmp <- links
    for (j in seq.int(length(links))) {
      if (length(scannedLinks) >= n) {
        break
      }
      if (!pb$finished) pb$tick()
      link <- links[[j]]
      if (!(link %in% scannedLinks)) {
        pageLinks <- extractor(link)
        pageLinks = pageLinks[which(startsWith(pageLinks, path))]
        pageLinks = lapply(pageLinks, function(x) {
          paste(url, x, sep = "")
        })
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
  links = unlist(links)
  if(beep) beep()
  links
}

#Parser
timestamp <- Sys.time()
months <- timestamp <- as.integer(Sys.time())
months <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")

parser <- function(links, limit = 0, step = 1, beep = F) {
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
  pb <- progress_bar$new(total = l, format = ":spin [:bar] :percent : :elapsed/:eta")
  time <- Sys.time()
  
  for(i in seq.int(l)) {
    link <- links[[i]]
    html <- read_html(link)
    
    html_time <- as.numeric(Sys.time())
    print("HTML")
    print((html_time - startTime)*1000)
    
    #Social networks
    df$social[i] <- getSocial(html)
    social_time <- as.numeric(Sys.time())
    print("Social")
    print((social_time - html_time)*1000)
    
    #Title
    df$title[i] <- getTitle(html)
    title_time <- as.numeric(Sys.time())
    print("Title")
    print((title_time - social_time)*1000)
    
    #Text
    df$text[i] <- getText(html)
    text_time <- as.numeric(Sys.time())
    print("Text")
    print((social_time - text_time)*1000)
    
    #Tags
    df$postTags[i] <- getTags(html)

    #Views
    df$views[i] <- getViews(html)
    
    #Comments
    df$comments[i] <- getComments(html)
    
    #Time
    df$time[i] <- getTime(html)
    
    pb$tick()
  }
  df <- subset(df, !is.na(title))
  df$id <- seq.int(length(df$url))
  if (beep) beep() 
  df[1:length(df$title),]
}

getSocial <- function(html) {
  sum(as.integer(html_text(html_nodes(html, "#twitterResult,#liResult,#fbResult,#reResult,#goResult,#emResult"))))
}

getTitle <- function(html) {
  html_text(html_node(html, ".left.fullWidth:not(.left.fullWidth.upper.fontColorOne)"))
}

getText <- function(html) {
  paste(html_text(html_nodes(html, "#ctl00_ctl00_ConMainBody_ConMainBody_ctl01_pnlBody,strong.fullWidth")))
}

getTags <- function(html) {
  paste(html_text(html_nodes(html, "#ctl00_ctl00_ConMainBody_ConMainBody_ctl01_lblInfo")), collapse = ",")
}

getViews <- function(html) {
  unlist(strsplit(html_text(html_node(html, "#ctl00_ctl00_ConMainBody_ConMainBody_ctl01_lblInfo")), "[[:space:]]"))[7]
}

getComments <- function(html) {
  unlist(strsplit(html_text(html_node(html, "#ctl00_ctl00_ConMainBody_ConMainBody_ctl01_lblInfo")), "[[:space:]]"))[12]
}

getTime <- function(html) {
  stats <- unlist(strsplit(html_text(html_nodes(html, "#ctl00_ctl00_ConMainBody_ConMainBody_ctl01_lblInfo")), "[[:space:]]"))
  if (stats[2] == "hours" || stats[2] == "hour") {
    hours <- as.integer(stats[1])
    time <- timestamp - hours * 3600
  } else if (stats[2] == "minutes" || stats[2] == "minute") {
    minutes <- as.integer(stats[1])
    time <- timestamp - minutes * 60
  } else {
    year <- as.integer(stats[3])
    month <- which(months == stats[2])
    day <- as.integer(stats[1])
    date <- ISOdate(year, month, day)
    time <- as.integer(date)
  }
  time
}