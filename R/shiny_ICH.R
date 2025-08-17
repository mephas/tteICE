#' @title Shiny for ICHe9r1
#'
#' @description This function opens the Rshiny app for ICHe9r1
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
  runApp(system.file("shiny", package = "ICHe9r1"))
}
