# Function to retrieve meta data (column definitions) for test directory shiny apps
#' @export
meta_cols <- function(meta_data, app) {
  meta_vars <- shinyfind::get_data(meta_data, app)
  meta_vars
}

# Function that returns a vector of columns that can be filtered 
# via the pickergroup
#' @export
meta_cols_filtered <- function(meta_data, app) {
  filtered_cols <-
    meta_cols(meta_data, app) |>
    filter(filterable == TRUE)
  
  filtered_cols
}

# Function that maps a column id to its full description
#' @export
map_id <- function(id, meta_data, app) {
  desc <- 
    meta_cols(meta_data, app) |>
    filter(id == !! id) |>
    pull(description)
  
  desc
}
