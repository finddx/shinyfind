#' Universal Data Download
#'
#' Memoised Function, updates once a day
#'
#' @name get_data
get_data_ <- function(dataset, version = c("main", "preview"), repo = "FINDtestdirData", time = today_at_sunrise()) {

  version <- match.arg(version)

  branch <- if (version == "main") {
    version
  } else {
    paste0(version, "-", dataset)
  }

  url <- paste0(
    "https://raw.githubusercontent.com/dsbbfinddx/", repo, "/", branch, "/data/", dataset, ".csv"
  )
  readr::read_csv(url, show_col_types = FALSE)
}

#' @export
get_data <- memoise::memoise(get_data_)


