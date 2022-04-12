#' Universal Data Download
#'
#' Memoised Function, updates once a day
#'
#' @name get_data
get_data_ <- function(dataset,
                      version = getOption("find.data.version", "main"),
                      repo = "FINDtestdirData",
                      time = today_at_sunrise()
                      ) {

  version <- match.arg(version, choices = c("main", "preview"))

  branch <- if (version == "main") {
    version
  } else {
    paste0(version, "-", dataset)
  }

  url <- paste0(
    "https://raw.githubusercontent.com/finddx/", repo, "/", branch, "/data/", dataset, ".csv"
  )
  readr::read_csv(url, show_col_types = FALSE)
}

#' @export
get_data <- memoise::memoise(get_data_)


