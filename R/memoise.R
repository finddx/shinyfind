

#' Starts a new day at UTC 7am
#' 
#' This can be used as an update trigger in a memoised function
today_at_sunrise <- function() {
  as.Date(lubridate::now("UTC") - 7 * 3600)
}


