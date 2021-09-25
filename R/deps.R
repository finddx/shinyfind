# from https://github.com/rstudio/shinydashboard/blob/master/R/deps.R

# Add an html dependency, without overwriting existing ones
appendDependencies <- function(x, value) {
  if (inherits(value, "html_dependency"))
    value <- list(value)

  old <- attr(x, "html_dependencies", TRUE)

  htmltools::htmlDependencies(x) <- c(old, value)
  x
}

# Add dashboard dependencies to a tag object
addDeps <- function(x) {
  dashboardDeps <- list(
    htmltools::htmlDependency(
      name = "shinyfind",
      version = as.character(utils::packageVersion("shinyfind")),
      src = c(file = system.file(package = "shinyfind", "www")),
      stylesheet = c("styles.css", "imperial-urw.css")
    )
  )
  appendDependencies(x, dashboardDeps)
}
