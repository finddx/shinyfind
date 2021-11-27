#' Shiny module server for downloading data_all
#' @export
download_data_all_server <- function(id, include_codebook = TRUE) {
  
  moduleServer(id, function(input, output, session) {
    . <- get_data_all()
    data <- .$data_all
    
    output$o_download <- downloadHandler(
      filename = function() {
        paste("data-all", Sys.Date(), if (input$i_download_type == "csv") ".csv" else ".xlsx", sep = "")
      },
      content = function(con) {
        if (input$i_download_type == "csv") {
          readr::write_csv(data, con)
        } else {
          if (isTRUE(include_codebook)) {
            codebook <-
              tibble::tibble(variable = colnames(data)) |>
              dplyr::left_join(get_codebook_extended(), by = "variable")
            writexl::write_xlsx(tibble::lst(data, codebook), con)
          } else {
            writexl::write_xlsx(data, con)
          }
        }
      }
    )
    
  })
}


#' Shiny module ui for downloading data_all
#' @export
download_data_all_ui <- function(id = NULL) {
  ns <- NS(id)
  fluidRow(
    column(
      width = 12,
      br(),
        h3("Download Raw Data"),
        tags$span(style = "display:flex;",
          "All data is available on our ",
          a(href = "https://github.com/dsbbfinddx/data/tree/master/processed", target = "_blank", "GitHub Repository."),
          "A",
          a(href = "https://github.com/dsbbfinddx/FINDCov19TrackerData/blob/master/processed/codebook.csv", target = "_blank", "Variable codebook"),
          "is also available.", p(
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
      download_data_ui("test")
    ),
    server = function(input, output, session) {
      # test w a dataset that has a column from codebook
      download_data_server("test")
    }
  )
}

# example_download_data()












