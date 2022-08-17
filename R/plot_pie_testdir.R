# Function for plotting pie charts in test directory shiny apps with a uniform
# style
#' @export
plot_pie <- function(data, id, subtitle = "Source: https://www.finddx.org/test-directory", ...) {
  
  validate(need(nrow(data) > 0, "No data to show. Please choose different filters"))
  
  vars <-
    data |>
    pull(!! id)
  
  factors <- na.omit(unique(vars))
  
  x <- list(id, factors)
  
  #data$color <- unlist(purrr::pmap(x, map_color))
  
  data_pie <-
    data |>
    rename(value = !! id ) |>
    count(value) |>
    filter(!is.na(value)) |>
    rename(`Number of tests` = n)
  
  data_pie$color <- unlist(purrr::pmap(x, map_color))
  
  data_pie |>
    e_charts(value) |>
    e_pie(`Number of tests`, radius = c("50%", "70%"), left = "80") |>
    e_toolbox_feature(feature = "saveAsImage", title = "Save", name = "FIND Plot") |>
    e_labels(show = FALSE) |>
    e_tooltip() |>
    e_legend(left = "0",
             top = "30px",
             type = "scroll", 
             orient = "vertical",
             textStyle = list(width = "140", fontSize = "11", overflow = "break")
    ) |>
    e_add_nested("itemStyle", color) |>
    e_title(
      text = map_id(id),
      subtitle = "Number of tests"
    ) |>
    add_logo(bottom = 0, subtitle = subtitle, height = 20)
  
}
