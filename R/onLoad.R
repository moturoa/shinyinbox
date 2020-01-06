#' Add JS paths to shiny
#'
#' @importFrom shiny addResourcePath
#'
#' @noRd
#'
.onLoad <- function(...) {
  shiny::addResourcePath('shinyinbox', system.file("assets", package = "shinyinbox"))
}
