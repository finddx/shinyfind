#' @examples 
#'
#'page_title <- "Test Directory"
#'extra_html <- '<p>This fully searchable directory currently lists all 
#'                  commercially available immunoassays and molecular tests
#'                </p>'
#'
#'ui <- find_dashboard_page(
#'  banner = find_banner(page_title, extra_html = extra_html),
#'  variants_ui("variants")
#' )
#' 
#'ui <- find_dashboard_page(
#'  title = "Test directory"
#'  variants_ui("variants")
#' )
#' @export
find_banner <- function(title, extra_html = NULL) {
  open_section <- '<section class="hero">
                     <span class="gradient">
                     </span>'
  
  heading <- paste('<div class="content"><h1>', 
                   toupper(title),
                   '</h1>')
  
  subtext <- paste0(extra_html, "</div>")
  close_section <- '</section>'
  full_html <- paste0(open_section, heading, subtext, close_section)
  htmltools::HTML(full_html)
}

#' @export
find_head <- function() {
  tags$head(
    tags$link(href = "https://fonts.googleapis.com/css2?family=Roboto:300")
  )
}

#' @export
find_dashboard_page <- function(..., title = NULL, banner = find_banner(title)) {
  addDeps(bootstrapPage(
    find_head(),
    banner,
    fluidPage(
      ...,
      br(),
      br(),
      title = title
    )
  ))
}
