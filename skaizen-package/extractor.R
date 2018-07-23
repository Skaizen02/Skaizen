#' Extractor Function
#' 
#' This function parses a page for any links
#' @param page the url of the page to parse

library(rvest)

#Link Extraction
extractor = function(html) {
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