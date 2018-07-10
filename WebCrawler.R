library(Rcrawler)
library(rvest)
library(tm)
library(beepr)

url <- "https://www.finextra.com/"
path <- "https://www.finextra.com/newsarticle"

#Crawler
crawler <- function(iterations, url, path, step = 10) {
  links <- list(url)
  scannedLinks <- c()
  for (i in 0:iterations) {
    tmp <- links
    for (j in seq.int(length(links))) {
      if (length(scannedLinks)%%step == 0 ) {
        print(sprintf("%s/%s : %s/%s", i, iterations, length(scannedLinks), length(tmp)))
      }
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
  links <- unlist(links)
  links
}

#Parser
selector <-   ".left.fullWidth:not(.left.fullWidth.upper.fontColorOne),
              .ncMetaDataSnippet,
              #ctl00_ctl00_ConMainBody_ConMainBody_ctl01_lblInfo,
              #twitterResult,
              #liResult,
              #fbResult,
              #reResult,
              #goResult,
              #emResult"

months <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")

parser <- function(links, limit = 0, step = 1, beep = T) {
  startTime <- as.integer(Sys.time())
  df <- data.frame(url = unlist(links))
  
  df$twitter <- NA
  df$linkedin <- NA
  df$facebook <- NA
  df$reddit <- NA
  df$google <- NA
  df$mail <- NA
  df$title  <- NA
  df$postTags <- NA
  df$time <- NA
  
  if (limit == 0) {
    l <- length(links)
  } else {
    l <- limit
  }
  
  time <- Sys.time()
  
  for(i in seq.int(l)) {
    link <- links[[i]]  
    html <- read_html(link)
    
    data <- html_text(html_nodes(html, selector))
    
    #Social networks
    df$twitter[i] <- as.integer(data[1])
    df$linkedin[i] <- as.integer(data[2])
    df$facebook[i] <- as.integer(data[3])
    df$reddit[i] <- as.integer(data[4])
    df$google[i] <- as.integer(data[5])
    df$mail[i] <- as.integer(data[6])
    
    #Title
    df$title[i] <- data[6]
    
    #Tags
    if (length(data) > 8) {
      df$postTags[i] <- paste(data[9:length(data)], collapse = ",")
    }
    
    #Stats
    stats <- unlist(strsplit(data[8], "[[:space:]]"))

    #Views
    df$views[i] <- as.integer(stats[7])
    
    #Comments
    df$comments[i] <- as.integer(stats[12])
    
    #Time
    timestamp <- as.integer(Sys.time())
    if (stats[2] == "hours" || stats[2] == "hour") {
      hours <- as.integer(stats[1])
      df$time[i] <- time - hours * 3600
    } else if (stats[2] == "minutes" || stats[2] == "minute") {
      minutes <- as.integer(stats[1])
      df$time[i] <- time - minutes * 60
    } else {
      year <- as.integer(stats[3])
      month <- which(months == stats[2])
      day <- as.integer(stats[1])
      date <- ISOdate(year, month, day)
      df$time[i] <- as.integer(date)
    }
    
    #Progress information
    if (i %% step == 0) {
      avgTime <- (as.integer(Sys.time()) - startTime) / i
      remainingTime <- avgTime * (l-i)
      seconds <- round(remainingTime %% 60)
      minutes <- round((remainingTime - seconds) / 60)
      progress_bar <- paste(c("[", lapply(seq.int(25), function(x, progress) {
        if (x <= progress) {
          "#"
        } else {
          " "
        }
      }, round(i/l*25)), "] "), sep = "", collapse = "")
      stepindicator <- sprintf("%s/%s", i, l)
      timeEstimate <- sprintf(" (%s minutes %s seconds)", minutes, seconds)
      print(paste(c( progress_bar, stepindicator, timeEstimate), sep = " ", collapse = ""))
    }
  }
  df <- subset(df, !is.na(title))
  df$id <- seq.int(length(df$url))
  if (beep) {
    beep() 
  }
  df[1:length(df$title),]
}

go <- function() {
  links <- crawler(2, url, path)
  df <- parser(links, 100)
}