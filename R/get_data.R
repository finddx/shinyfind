#' Universal Data Download
#'
#' Memoised Function, updates once a day
#'
#' @name get_data
get_data_ <- function(dataset,
                      app = NULL,
                      version = getOption("find.data.version", "main"),
                      repo = "FINDtestdirData",
                      time = today_at_sunrise()
                      ) {
  if(is.null(app)) stop("Specify the app you are requesting its data. e.g covid19")
  
  version <- match.arg(version, choices = c("main", "preview","report"))

  branch <- if (version == "main" | grepl("-", version)==FALSE) {
    version
  } else {
    paste0(version, "-", dataset)
  }

  url <- paste0(
    "https://raw.githubusercontent.com/finddx/", repo, "/", branch, "/data/", app, "/", dataset, ".csv"
  )
  readr::read_csv(url, show_col_types = FALSE)
}

#' @export
get_data <- memoise::memoise(get_data_)


