#' Universal Data Download
#'
#' Memoised Function, updates once a day
#'
#' @name get_data
get_data_ <- function(dataset, repo = "FINDtestdirData", branch = "main", time = today_at_sunrise()) {
  url <- paste0(
    "https://raw.githubusercontent.com/dsbbfinddx/", repo, "/", branch, "/data/", dataset, ".csv"
  )
  readr::read_csv(url, show_col_types = FALSE)
}

#' @export
get_data <- memoise::memoise(get_data_)


