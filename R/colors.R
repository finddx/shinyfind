# No color codes should be found outside this file

# library(colorblindr)
# gg_color_swatches(8) +
#   scale_fill_manual(values = pal_find())
pal_find <- function(n = 7) {
  stopifnot(n <= 7)
  find_colors <- c(
    "#D2EAED",
    "#43abb6",
    "#F3D1D4",
    "#cd4652",
    "#F3D1D4",
    "#602B59"
  )
  find_colors[1:n]
}
