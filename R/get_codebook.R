
# The codebook in `FINDCov19TrackerData/processed/codebook.csv` is a subset of the
# main codebook, which is in `codebook/codebook_extended.csv`. Please update
# there!


# To update FINDCov19TrackerData/processed/codebook.csv, use this:
#
# shinyfind::get_codebook() |>
#   select(col_type, variable) |>
#   left_join(shinyfind::get_codebook_extended(), by = c("col_type", "variable")) |>
#   write_csv("../FINDCov19TrackerData/processed/codebook.csv")


#' Get Codebook
#'
#' Memoised Function, updates once a day
#'
#' @name get_codebook
get_codebook_ <- function(time = shinyfind:::today_at_sunrise()) {
  codebook <- readr::read_csv("https://raw.githubusercontent.com/finddx/FINDCov19TrackerData/master/processed/codebook.csv", col_types = readr::cols())
  codebook
}

#' @export
get_codebook <- memoise::memoise(get_codebook_)


#' @export
#' @name get_codebook
get_codebook_extended <- function(show_cacluation = FALSE) {
  ans <- readr::read_csv(system.file(package = "shinyfind", "codebook/codebook_extended.csv"), col_types = "c")

  if (show_cacluation == FALSE) {
    ans <- dplyr::select(ans, col_type, variable, description ,description_short)
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
      "https://raw.githubusercontent.com/finddx/FINDCov19TrackerData/master/raw/country_info.csv",
      col_types = readr::cols()
    )
  country_info
}

#' @export
get_country_info <- memoise::memoise(get_country_info_)
