#' Get Codebook
#'
#' Memoised Function, updates once a day
#'
#' @name get_codebook
get_codebook_ <- function(time = shinyfind:::today_at_sunrise()) {
  codebook <- readr::read_csv("https://raw.githubusercontent.com/dsbbfinddx/FINDCov19TrackerData/master/processed/codebook.csv", col_types = readr::cols())
  codebook
}

#' @export
get_codebook <- memoise::memoise(get_codebook_)


#' @export
#' @name get_codebook
get_codebook_extended <- function(show_cacluation = FALSE) {
  ans <- readr::read_csv(system.file(package = "shinyfind", "codebook/codebook_extended.csv"), col_types = "c")

  if (show_cacluation == FALSE) {
    ans <- dplyr::select(ans, -calculation)
  }

  ans
}


#' Get Country Info
#'
#' Memoised Function, updates once a day
#'
#' @name get_codebook
get_country_info_ <- function(time = shinyfind:::today_at_sunrise()) {
  country_info <- readr::read_csv(
      "https://raw.githubusercontent.com/dsbbfinddx/FINDCov19TrackerData/master/raw/country_info.csv",
      col_types = readr::cols()
    )
  country_info
}

#' @export
get_country_info <- memoise::memoise(get_country_info_)
