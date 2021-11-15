#' Shiny module server for downloading data
#' @export
download_data_server <- function(id, data, include_codebook = TRUE) {
  
  moduleServer(id, function(input, output, session) {
    output$o_download <- downloadHandler(
      filename = function() {
        paste("data-", Sys.Date(), if (input$i_download_type == "csv") ".csv" else ".xlsx", sep = "")
      },
      content = function(con) {
        if (input$i_download_type == "csv") {
          readr::write_csv(data, con)
        } else {
          if (isTRUE(include_codebook)) {
            codebook <-
              tibble(variable = colnames(data)) |>
              dplyr::left_join(get_codebook(), by = "variable")
            writexl::write_xlsx(lst(data, codebook), con)
          } else {
            writexl::write_xlsx(data, con)
          }
        }
      }
    )
    
  })
}


#' Shiny module ui for downloading data
#' @export
download_data_ui <- function(id = NULL) {
  ns <- NS(id)
  
  tags$span(style = "display:flex;", p(
    downloadLink(ns("o_download"), label = "Download Data"), tags$span(style = "margin-left: 15px"),
    prettyRadioButtons(
      inputId = ns("i_download_type"),
      label = NULL,
      status = "default",
      inline = T,
      choices = c(
        "CSV" = "csv",
        "Excel" = "xlsx"
      )
    )
  ))
}


# run as a module within minimal app
example_download_data <- function() {
  library(shiny)
  library(shinyWidgets)
  shiny::shinyApp(
    ui = find_dashboard_page(
     download_data_ui("test")
    ),
    server = function(input, output, session) {
      # test w a dataset that has a column from codebook
      download_data_server("test", data = dplyr::mutate(cars, cap_cum_cases = 1))
    }
  )
}

# example_download_data()












