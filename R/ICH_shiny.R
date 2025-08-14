##' @title Rshiny for ICHe9r1
##'
##' @return shiny interface
##'
##'
##' @examples
##' if (interactive()) {
##'  ICHe9r1Shiny()
##' }

#' @export
ICHe9r1Shiny <- function() {
  suppressPackageStartupMessages(suppressMessages(
    shiny::runApp(system.file("6MFSanova", package = "mephas"))
  ))
}
