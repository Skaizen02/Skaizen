day_preprocessing = function(df) {
  
  wdays = as.POSIXlt(df$time, origin = "1970-01-01")$wday
  
  wdaydf <- data.frame(to_categorical(wdays, 7))
  colnames(wdaydf) <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
  
  df <- cbind(df, wdaydf)
  df
}
