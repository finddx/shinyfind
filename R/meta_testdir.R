# Function to retrieve meta data (column definitions) for test directory shiny apps
#' @export
meta_cols <- function(data) {
  meta_vars <- shinyfind::get_data(data)
  meta_vars
}

# Function that returns a vector of columns that can be filtered 
# via the pickergroup
#' @export
meta_cols_filtered <- function() {
  filtered_cols <-
    meta_cols() |>
    filter(filterable == TRUE)
  
  filtered_cols
}

# Function that maps a column id to its full description
#' @export
map_id <- function(id) {
  desc <- 
    meta_cols() |>
    filter(id == !! id) |>
    pull(description)
  
  desc
}
