#' Shiny module server for downloading data
#' @export
download_data_server <- function(id, data) {
  
  moduleServer(id, function(input, output, session) {
    
    data_download <- data
    type <- input$i_download_all
    
    output$o_download_all <- downloadHandler(
      filename = function() {
        paste("data-", Sys.Date(), if (type == "csv") ".csv" else ".xlsx", sep = "")
      },
      content = function(con) {
        if (type == "csv") {
          write_csv(data_download, con)
        } else {
          writexl::write_xlsx(data_download, con)
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
    "All the data can be downloaded ", downloadLink(ns("o_download_all"), label = "here."), tags$span(style = "margin-left: 15px"),
    prettyRadioButtons(
      inputId = ns("i_download_all"),
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
#ui <- find_dashboard_page(
#  download_data_ui("test")
#)

#server <- function(input, output, session) {
#  download_data_server("test")
#}

# shinyApp(ui = ui, server = server)








