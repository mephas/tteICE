#' @title Print the main results of tteICE
#'
#' @description This function summarize the results
#'
#' @param x A fitted object from \code{surv.ICH} or \code{scr.ICH}.
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
#' ## plot treatment effects with p-values
#' for (st in c('composite','natural','removed','whileon','principal')){
#' fit = surv.ICH(A, bmt$t2, bmt$d4, st)
#' print(fit)
#' }
#' ## plot counterfactual cumulative incidence functions
#' for (st in c('composite','natural','removed','whileon','principal')){
#' fit = surv.ICH(A, bmt$t2, bmt$d4, st)
#' print(fit, digits=2)
#' }
#'
#' @seealso
#' \code{\link{surv.ICH}},
#' \code{\link{scr.ICH}}
#'
#'
#' @method print ICH
#' @export

print.ICH <- function(x, digits=3, ...){

  if(!inherits(x, "ICH")) stop("Only valid for models by tteICE.")
  # ate.q = round(quantile(x$ate, probs=c(0.25,0.5,0.75)), digits)
  if(is.null(x$p.val)) p=NA else p=x$p.val
  cat("The P-value of ATE by strategy", x$strategy, "using", x$method, "estimation method:", round(p, digits), "\n")
  #cat("\nThe quatiles (25%, 50%, 75%) of ATE:", paste(ate.q, collapse = ", "), "\n")
  cat("-------------------------------------------------------------------\n")
  cat("The estimated cumulative incidences and treatment effects at quartiles:\n")
  cat(round(riskpredict(x), digits),"\n")
  invisible(x)

}
