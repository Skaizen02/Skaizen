library(XML)
library(readtext)

parseHTML <- function(filename) {
  file <- readtext(file = filename)
  file
  # rbind(data.frame(doc = doc.html), df)
}

df <- data.frame(doc=c())
names(df) <- c("doc")
parsed <- NULL

dirs <- list.dirs(path = "pages")

files <- lapply(dirs, function(dir) {
  filenames <- list.files(path = dir, pattern = "*.html")
  lapply(filenames, function(x, dir) paste(dir, x, sep = "/"), dir)
})

files <- unlist(files)

docs <- lapply(files, function(x) parseHTML(x))
