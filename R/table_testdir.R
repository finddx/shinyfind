#' Lists of Test Table for test directory apps
#'
#' @examples
#'
#' table_tests(session, data, meta_name)
#' @export
table_tests <- function(session, data, meta_name) {
  #meta_name <- 'testdir_meta_cols_proof'
  x <- meta_cols(meta_name)
  
  col_desc <- purrr::map2(names(data), meta_name, map_id)
  col_names <- setNames(names(data), col_desc)
  #action <- DT::dataTableAjax(session, data)
  DT::datatable(data,
                rownames = FALSE,
                colnames = col_names,
                options = list(
                  initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'font-family':'Roboto, sans-serif', 'font-size':'13px'});",
                    "$('.dataTables_filter input[type=search]').css({'width': '100%', 'display': 'inline-block', 'position':'absolute'});",
                    "var el_id = $('.dataTables_filter').attr('id')",
                    "$('#' + el_id).find('label').contents().eq(0).replaceWith('');",
                    "}"),
                  dom = 'Bfrtip',
                  language = list(searchPlaceholder = "Search test data..."),
                  pageLength = 10, 
                  columnDefs = list(
                    list(className = 'dt-center', targets ="_all",
                         render = JS(
                           "function(data, type, row, meta) {",
                           "return data === null ? '-' : data;",
                           "}"))
                  )), escape = FALSE) |>
    DT::formatStyle(
      columns = unlist(col_desc),
      color = "black",
      fontFamily = "Roboto, sans-serif",
      fontSize = "13px",
      fontWeight = "normal",
      lineHeight = "normal",
      paddingBottom = "15px",
      paddingLeft = "5.2px",
      paddingRight = "5.2px",
      paddingTop = "15px",
      textAlign = "center",
      verticalAlign = "top")
  
}
