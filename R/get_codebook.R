#' Get Codebook
#'
#' Memoised Function, updates once a day
#'
#' @name get_meta
get_codebook_ <- function(time = shinyfind:::today_at_sunrise()) {
  codebook <- readr::read_csv("https://raw.githubusercontent.com/dsbbfinddx/FINDCov19TrackerData/master/processed/codebook.csv", col_types = readr::cols())
  codebook
}

#' @export
get_codebook <- memoise::memoise(get_codebook_)





