#' Crawler Function
#' 
#' This function finds pages following links from the start page given and parses them to gather data.
#' @param domain The domain to search iside of
#' @param path The beginning of the path of the page you want
#' @param start The page to start from
#' @param n The amount of pages to parse. Defaults to 100.
#' @param beep Makes a beep when finished when set to TRUE. Defauts to FALSE.
#' @export
#' @examples
#' df <- crawler("https://www.finextra.com", "/newsarticle", "/32414/russian-bank-loses-1-million-to-hackers-who-compromised-an-outdated-router", 1000, T)

library(rvest)
library(beepr)
library(progress)

crawler <- function(domain, path, start, n = 100, beep = F) {
  links <- list()
  link <- paste(domain, path, start, sep = "")
  df <- NULL
  pb <- progress_bar$new(total = n, format = ":spin [:bar] :percent : :elapsed/:eta")
  for (i in 1:n) {
    if (!pb$finished) pb$tick()
    
    tryCatch({
      parsed <- parser(link)
      pageLinks <- parsed$links
      
      pageLinks <- pageLinks[which(startsWith(pageLinks, path))]
      pageLinks <- lapply(pageLinks, function(x) {
        paste(domain, x, sep = "")
      })
      
      links <- unique(c(links, pageLinks))
      
      data <- parsed$data
    }, error = function(err) {
      data <- list()
      
      print(err)
      print(link)
    })
    
    if (is.null(df)) {
      df <- data.frame(data)
    } else {
      df <- rbind(df, data.frame(data))
    }
    
    link <- links[[i]]
  }
  if(beep) beep()
  df
}