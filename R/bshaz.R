#' @title S3 method of baseline hazards
#'
#' @description This function extracts the baseline cumulative hazards in the survival models
#'
#' @param x
#' A fitted object returned by the function \code{tteICE}, \code{surv.tteICE}, or \code{scr.tteICE}.
#'
#' @return
#' A data frame of baseline cumulative hazards in the working Kaplan-Meier or Cox models, stratified by treatment groups from the model object.
#'
#' @keywords internal
#' @export
bshaz <- function(x) {
  UseMethod("bshaz")
}
