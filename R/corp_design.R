#' @export
find_banner <- function(title) {
  htmltools::HTML('
    <section class="hero"><span class="gradient"></span>
        <div class="content">
            <h2 class="subtitle underline">', toupper(title),'</h2>

        </div>
    </section>
  ')
}

#' @export
find_head <- function() {
  tags$head(
    # tags$link(href = "imperial-urw.css", rel = "stylesheet"),
    # tags$link(href = "styles.css", rel = "stylesheet"),

    # add a 2nd time, this must be afeter dashboard.css
    tags$link(href = paste0("shinyfind-", packageVersion("shinyfind"), "/imperial-urw.css"), rel = "stylesheet"),
    tags$link(href = paste0("shinyfind-", packageVersion("shinyfind"), "/styles.css")), rel = "stylesheet",
    tags$link(href = "https://fonts.googleapis.com/css2?family=Roboto:300")
  )
}

#' @export
find_dashboard_page <- function(..., title = "") {
  tablerDash::tablerDashPage(
    navbar = tags$div(
      find_banner(title)
    ),
    loading_duration = 0,
    footer = tablerDash::tablerDashFooter(tagList(title), copyrights = "Copyright FIND"),
    title = title,
    body = addDeps(tablerDash::tablerDashBody(
      find_head(),
      ...
    ))
  )
}
