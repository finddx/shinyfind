#' @export
find_banner <- function(title, extra_html = NULL) {
  open_section <- htmltools::HTML('
    <section class="hero"><span class="gradient"></span>
  ')
  
  heading <- htmltools::HTML('
    
        <div class="content">
            <h1>', toupper(title),'</h1>
        </div>
    
  ')
  
  subtext <- htmltools::HTML('<p> Na wa </p>')
  close_section <- htmltools::HTML('</section>')
  
  paste(open_section, heading, subtext, close_section)
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
