# No color codes should be found outside this file

# library(colorblindr)
# gg_color_swatches(8) +
#   scale_fill_manual(values = pal_find())

#' @export
pal_find <- function(n = 14) {
  stopifnot(n <= 14)
  find_colors <- c(
    # 100% shade of 3 primary and 1 secondary FIND color
    "#5b254e",
    "#00a2ab",
    "#7b97a0",
    "#e64148",
    # randomly picked from FIND color strips
    "#306e7c",
    "#9b2c4c",
    # 85% shade of 3 primary and 1 secondary FIND color
    "#703d5f",
    "#00b0b7",
    "#91a5ad",
    "#ea645d",
    # 65% shade of 3 primary colors and 1 secondary FIND color
    "#8f637b",
    "#6dc3c8",
    "#adbac0",
    "#f08d80",
    # others
    "#01A2AB",
    "#F19576",
    "#E85239"
  )
  find_colors[1:n]
}


# as discussen in GitHub....
# gg_color_swatches(5) +
#   scale_fill_manual(values = pal_find_regions())
#' @export
pal_find_regions <- function(n = 6) {
  stopifnot(n <= 6)
  find_regions <- c(
    "#ffab00", # africa
    "#e3374b", # asia
    "#8c9eab", # europe
    "#5e2e58", # north america
    "#43abb6", # oceania
    "#9c2e4e"  # south america
    
  )
  find_regions[1:n]
}


# from https://public.flourish.studio/story/1069343/?full
# gg_color_swatches(5) +
#   scale_fill_manual(values = pal_find_variants())
#' @export
pal_find_variants <- function(n = 6) {
  stopifnot(n <= 6)
  find_variants <- c(
    "#7B97A0",  # unknown
    "#6DC3C8",  # no expected impact (in silico analyses)
    "#01A2AB", # no impact  (analytical/clinical studies conducted)
    "#F19576",  # potential impact (in silico analyses)
    "#E85239",  # impact (analytical/clinical studies)
    "#adbac0"  # not applicable
  )
  find_variants[1:n]
}


#' @export
pal_find_green_first <- function() {
  c(shinyfind::pal_find()[2], shinyfind::pal_find()[1], shinyfind::pal_find()[4], shinyfind::pal_find()[-c(1,2,4)])
}


# FIND extended palette
# gg_color_swatches(12) +
#   scale_fill_manual(values = pal_find_extended())
#' @export
pal_find_extended <- function(n = 12) {
  stopifnot(n <= 12)
  find_colors_ext <- c(
    "#491E5D",
    "#489FA9",
    "#81969F",
    "#D44F4E",
    "#C7C5A7",
    "#FF8F28",
    "#5A9A70",
    "#FFCB00",
    "#354159",
    "#9C9AFF",
    "#865345",
    "#9AE9BA"
  )
  find_colors_ext[1:n]
}
