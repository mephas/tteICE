#' @title Print a short summary of the estimated treatment effect
#'
#' @description This function summarize the results
#'
#' @param x A fitted object returned by the function \code{surv.tteICE} or \code{scr.tteICE}.
#'
#' @param digits The digits of the results
#'
#' @param ... Other augments in function \code{\link{print.default}}
#'
#' @importFrom stats quantile
#'
#' @examples
#' ## load data
#' data(bmt)
#' bmt = transform(bmt, d4=d2+d3)
#' A = as.numeric(bmt$group>1)
#'
#' ## print the results
#' for (st in c('composite','natural','removed','whileon','principal')){
#' fit = surv.tteICE(A, bmt$t2, bmt$d4, st)
#' print(fit)
#' }
#' for (st in c('composite','natural','removed','whileon','principal')){
#' fit = scr.tteICE(A, bmt$t1, bmt$d1, bmt$t2, bmt$d2, st)
#' print(fit, digits=2)
#' }
#'
#' @seealso
#' \code{\link[tteICE]{surv.tteICE}},
#' \code{\link[tteICE]{scr.tteICE}}
#'
#' @method print tteICE
#' @return Print the summary of a tteICE object
#' @export

print.tteICE <- function(x, digits=3, ...){

  if (!inherits(x, "tteICE"))  stop("Must be an object returned by `surv.tteICE` or `scr.tteICE`.", call. = FALSE)
  # ate.q = round(quantile(x$ate, probs=c(0.25,0.5,0.75)), digits)
  if(is.null(x$p.val)) p=NA else p=x$p.val
  cat("The P-value of the estimated treatment effect by strategy", x$strategy, "using", x$method, "estimation method:", round(p, digits), "\n")
  #cat("\nThe quatiles (25%, 50%, 75%) of ATE:", paste(ate.q, collapse = ", "), "\n")
  cat("-------------------------------------------------------------------\n")
  cat("The estimated cumulative incidences and treatment effects at quartiles:\n")
  print(round(riskpredict(x), digits))
  invisible(x)
}
