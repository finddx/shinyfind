
#' Shiny module ui for downloading data_all
#' @export
download_data_raw_ui <- function(id = "data_raw") {
  ns <- NS(id)
  fluidRow(
    column(
      width = 12,
        br(),
        h3("Download Raw Data"),
        tags$span(style = "display:flex;", p(
          "All data is available on our ",
          a(href = "https://github.com/dsbbfinddx/data/tree/master/processed", target = "_blank", " GitHub Repository."),
          " A ",
          a(href = "https://github.com/dsbbfinddx/FINDCov19TrackerData/blob/master/processed/codebook.csv", target = "_blank", " Variable codebook"),
          "is also available. "
        )
      )
    )
  )
}


# run as a module within minimal app
example_download_data <- function() {
  library(shiny)
  library(shinyWidgets)
  shiny::shinyApp(
    ui = find_dashboard_page(
      download_data_ui()
    ),
    server = function(input, output, session) {
      # no need for a content here
    }
  )
}

# example_download_data()












