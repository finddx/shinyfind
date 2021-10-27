#' Get Codebook
#'
#' Memoised Function, updates once a day
#'
#' @name get_meta
get_codebook_ <- function(time = shinyfind:::today_at_sunrise()) {
  codebook <- read_csv("https://raw.githubusercontent.com/dsbbfinddx/FINDCov19TrackerData/master/processed/codebook.csv", col_types = cols())
  codebook
}

#' @export
get_codebook <- memoise::memoise(get_codebook_)





