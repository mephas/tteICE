#' @title S3 method to checking proportional hazards
#'
#' @description This function checks the proportional hazards assumption in the Cox model using Schoenfeld residuals.
#' This function only return results for strategies based on efficient influence functions.
#'
#' @param x
#' A fitted object returned by the function \code{tteICE}, \code{surv.tteICE}, or \code{scr.tteICE}.
#'
#'
#' @return
#' A list of P-values of testing the proportional hazards (PH) assumption in the working Cox models, for each
#' covariate and a global test, stratified by treatment groups.
#'
#' @export
zph <- function(x) UseMethod("zph")


