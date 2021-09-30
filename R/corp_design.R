#' @export
find_banner <- function(title) {
  htmltools::HTML('
    <section class="hero"><span class="gradient"></span>
        <div class="content">
            <h1>', toupper(title),'</h1>
        </div>
    </section>
  ')
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
    title = title
  )
}
