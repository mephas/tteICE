#' @title Shiny for tteICE
#'
#' @description This function opens the Rshiny app for tteICE.
#'
#' @return Rshiny inteface
#' @import shiny shinythemes shinyWidgets
#' @importFrom DT renderDT
#' @importFrom psych describe
#'
#' @examples
#' if(interactive()){
#'   ICHe9r1Shiny()
#' }
#'
#'
#' @export

ICHe9r1Shiny <- function(){
  runApp(system.file("shiny", package = "tteICE"))
}
