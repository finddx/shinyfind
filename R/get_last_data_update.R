# Function to get the last date of update for any dataset on GitHub
# E.g of an endpoint: "https://api.github.com/repos/finddx/FINDCov19TrackerData/commits?path=processed%2Fdata_all.csv&page=1&per_page=1"
#' @export
get_last_data_update <- function(endpoint) {
  last_data_update <- curl::curl_fetch_memory(endpoint)$modified
}

