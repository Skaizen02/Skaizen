library(Rcrawler)
library(rvest)
library(tm)
library(beepr)

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
crawler <- function(iterations, url, path, step = 10, n = 100) {
  links <- list(url)
  pb <- txtProgressBar(style = 3, max = n)
  scannedLinks <- c()
  for (i in 0:iterations) {
    tmp <- links
    for (j in seq.int(length(links))) {
      # if (length(scannedLinks)%%step == 0 ) {
      setTxtProgressBar(pb, length(scannedLinks))
        # print(sprintf("%s/%s : %s/%s", i, iterations, length(scannedLinks), n))
      # }
      if (length(scannedLinks) >= n) {
        break
      }
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
  links
}

#Parser
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

parser <- function(links, limit = 0, step = 1, beep = T) {
  startTime <- as.integer(Sys.time())
  df <- data.frame(url = unlist(links))
  
  # df$twitter <- NA
  # df$linkedin <- NA
  # df$facebook <- NA
  # df$reddit <- NA
  # df$google <- NA
  # df$mail <- NA
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
  pb <- txtProgressBar(style=3, max=l)
  
  time <- Sys.time()
  
  for(i in seq.int(l)) {
    link <- links[[i]]  
    html <- read_html(link)
    
    data <- html_text(html_nodes(html, selector))
    
    #Social networks
    df$social[i] = sum(as.numeric(data[1:6]))
    # df$twitter[i] <- as.integer(data[1])
    # df$linkedin[i] <- as.integer(data[2])
    # df$facebook[i] <- as.integer(data[3])
    # df$reddit[i] <- as.integer(data[4])
    # df$google[i] <- as.integer(data[5])
    # df$mail[i] <- as.integer(data[6])
    
    #Title
    df$title[i] <- data[7]
    
    #Text
    df$text[i] = paste(data[9], data[10])
    
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
    # if (i %% step == 0) {
    #   avgTime <- (as.integer(Sys.time()) - startTime) / i
    #   remainingTime <- avgTime * (l-i)
    #   seconds <- round(remainingTime %% 60)
    #   minutes <- round((remainingTime - seconds) / 60)
    #   progress_bar <- paste(c("[", lapply(seq.int(25), function(x, progress) {
    #     if (x <= progress) {
    #       "#"
    #     } else {
    #       " "
    #     }
    #   }, round(i/l*25)), "] "), sep = "", collapse = "")
    #   stepindicator <- sprintf("%s/%s", i, l)
    #   timeEstimate <- sprintf(" (%s minutes %s seconds)", minutes, seconds)
    #   print(paste(c( progress_bar, stepindicator, timeEstimate), sep = " ", collapse = ""))
    # }
    setTxtProgressBar(pb, i)
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