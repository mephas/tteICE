#' @title Shiny app for tteICE
#'
#' @description This function opens the RShiny app for tteICE. RShiny application can be used for generating plots and
#' basic analysis results. It provides a point-and-click interface, so users can
#' obtain results without writing R code directly.
#'
#' @return Rshiny interface
#'
#' @import shiny shinythemes shinyWidgets
#'
#' @importFrom DT renderDT
#'
#' @importFrom psych describe
#'
#' @examples
#' \donttest{
#' if(interactive()){
#'   tteICEShiny()
#' }
#' }
#'
#' @export

tteICEShiny <- function(){
  runApp(system.file("shiny", package = "tteICE"))
}
