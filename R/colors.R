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
    "#f08d80"
  )
  find_colors[1:n]
}


# as discussen in GitHub....
# gg_color_swatches(4) +
#   scale_fill_manual(values = pal_find_regions())
pal_find_regions <- function() {
  find_regions <- c(
    "#e3374b",  # asia
    "#8c9eab",  # europe
    "#5e2e58",  # americas
    "#43abb6",  # oceania
    "#ffab00"   # africa
  )
  find_regions
}

