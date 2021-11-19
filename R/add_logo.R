# This function adds a FIND logo to an echarts plot
# For this to work, you must have a logo named 'logo_header.svg' in the
# www/ directory of your Shiny app or module

# Example:
# mtcars |>
# e_charts(cyl) |>
# e_bar(mpg) |>
# add_logo()


#' @export
add_logo <- function(e, image = "img/logo_header.svg", height = 25, top = "auto", bottom = 20, left = "right") {
  logo = list( 
    logo = list(height=height, backgroundColor=list(
      image = image))
  )
  e <- 
    e |> 
    e_title(text='{logo| }',
            left = left,
            bottom = bottom,
            top = top,
            textStyle = list(fontStyle = "normal", rich = logo),
            subtext = "Source: finddx.org/covid-19/test-tracker",
            itemGap = 1) 
  e
}
