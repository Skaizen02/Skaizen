# packages

library(httr)
library(jsonlite)
library(tidyverse)

filter_web_content <- function(query, sort = "relevancy",
                               ts = (Sys.time() - (3 * 24 * 60 * 60)),
                               order = "asc", size = 100, from = 0,
                               token = Sys.getenv("WEBHOSE_TOKEN")) {
  
  params <- list(
    token = token,
    format = "json",
    q = query,
    sort = sort,
    order = order,
    size = size,
    ts = ts
  )
  
  httr::GET(
    url = "https://webhose.io/filterWebContent",
    query = params
  ) -> res
  
  httr::stop_for_status(res)
  
  res <- httr::content(res, as = "text", encoding = "UTF-8")
  res <- jsonlite::fromJSON(res, flatten = TRUE)
  
  res
  
}

mcga <- function(tbl) {
  
  x <- colnames(tbl)
  x <- tolower(x)
  x <- gsub("[[:punct:][:space:]]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("(^_|_$)", "", x)
  x <- make.unique(x, sep = "_")
  
  colnames(tbl) <- x
  
  tbl
  
}

## Here's the setup bit:

## preallocate a list to avoid growing & copying. I have no idea if "30" is a good default for this API
## create a loop, increment both the list index variable and the API fetch starting position
## shove the results into the next empty slot in the list

PRE_ALLOC_MAX <- 30
results <- vector(mode = "list", length = PRE_ALLOC_MAX)

i <- 1
from <- 0
repeat {
  res <- filter_web_content("(China AND United ) language:english site_type:news site:bloomberg.com", 
                            ts = 1213456, from = from)
  results[[i]] <- res
  if (res[["moreResultsAvailable"]] > 0) {
    message("Fetching next 100 records...")
    i <- i + 1
    from <-  from + 100  
  } else {
    break
  }
}
## Fetching next 100 records...
## Fetching next 100 records...

## Now:
  
##  remove any NULL (unpopulated) entries
## extract the part of the API result with the data we need
## wrap it up in a data frame
## make the column names great again

discard(results, is.null) %>% 
  map_df(~{ .x$posts}) %>% 
  tbl_df() %>% 
  mcga()

## # A tibble: 227 x 42
##                                        uuid
##                                       <chr>
##  1 ea6f6084be16a50b0d4791ffa268956ca691c16d
##  2 bd0ac60981ac73e2a7e71378881272eb5b6147d7
##  3 3f2c2c13aa2b3c6d5fc8300f3a9876d9c86c08d1
##  4 659d73d3ddba3c0a0505da8fc15862bc33ac9519
##  5 371293cf38efe9c9a4708403c816c8b33eeb1298
##  6 38a3522fe1d268519aa0e2c3c865bbee19f9ee65
##  7 a4b1f0e4a8d94354ae41c80bebe56237b5a39ca8
##  8 323660c1c21662a1e5b147455f7a4c70f60e12b8
##  9 3233102dbbed6bd90c19ddb2cf7df9111de6ffcf
## 10 c4f126943968be899a6c5fdd806274f0ca848714
## # ... with 217 more rows, and 41 more variables: url <chr>, ord_in_thread <int>,
## #   author <chr>, published <chr>, title <chr>, text <chr>, highlighttext <chr>,
## #   highlighttitle <chr>, language <chr>, external_links <list>, rating <lgl>,
## #   crawled <chr>, thread_uuid <chr>, thread_url <chr>, thread_site_full <chr>,
## #   thread_site <chr>, thread_site_section <chr>, thread_site_categories <list>,
## #   thread_section_title <chr>, thread_title <chr>, thread_title_full <chr>,
## #   thread_published <chr>, thread_replies_count <int>,
## #   thread_participants_count <int>, thread_site_type <chr>, thread_country <chr>,
## #   thread_spam_score <dbl>, thread_main_image <chr>,
## #   thread_performance_score <int>, thread_domain_rank <int>,
## #   thread_social_facebook_likes <int>, thread_social_facebook_comments <int>,
## #   thread_social_facebook_shares <int>, thread_social_gplus_shares <int>,
## #   thread_social_pinterest_shares <int>, thread_social_linkedin_shares <int>,
## #   thread_social_stumbledupon_shares <int>, thread_social_vk_shares <int>,
## #   entities_persons <list>, entities_organizations <list>,
## #   entities_locations <list>

res <- fetch_posts("(China AND United) language:english site_type:news site:bloomberg.com",ts = 1213456)

dplyr::glimpse(res)

