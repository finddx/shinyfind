# Function for plotting bar charts in test directory shiny apps with a uniform
# style
#' @import echarts4r
#' @export
plot_bar <- function(data,
                     id,
                     meta_data = NULL,
                     app = NULL,
                     left = "130px",
                     nameGap = 100,
                     subtitle = "Source: https://www.finddx.org/test-directory/",
                     title_display = TRUE,
                     ...) {

  validate(need(nrow(data) > 0, "No data to show. Please choose different filters"))

  title_text <- if(title_display)  map_id(id, meta_data, app) else ''
  
  data |>
    rename(value = !! id ) |>
    count(value) |>
    rename(`Number of tests` = n) |>
    filter(!is.na(value)) |>
    arrange(desc(value)) |>
    e_charts(value, ...) |>
    e_bar(`Number of tests`) |>
    e_flip_coords() |>
    e_legend(show = FALSE) |>
    e_tooltip(trigger = "axis") |>
    e_y_axis(
      name = map_id(id, meta_data, app),
      nameGap = nameGap,
      nameLocation = "middle",
      axisLabel = list(
        interval = 0,
        width = "120",
        overflow = "break",
        color = "black"
      )
    ) |>
    e_x_axis(
      name = "Number of tests",
      nameLocation = "middle",
      nameGap = 30
    ) |>
    e_grid(left = left, right = "20px", top = "60px", bottom = "70px") |>
    e_toolbox_feature(feature = "saveAsImage", title = "Save", name = "FIND Plot") |>
    add_logo(bottom = 0, subtitle = subtitle, height = 20) |>
    e_theme_custom(as.character(jsonlite::toJSON(list(color = pal_find_green_first())))) |>
    e_title(
      text = title_text
    )
}
