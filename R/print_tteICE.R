#' @title Print a short summary of the estimated treatment effect
#'
#' @description This function summarizes the results
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
#' ## print the results
#' for (st in c('composite','natural','removed','whileon','principal')){
#'  fit = surv.tteICE(A, bmt$t2, bmt$d4, st)
#'  print(fit)
#' }
#' for (st in c('composite','natural','removed','whileon','principal')){
#'  fit = scr.tteICE(A, bmt$t1, bmt$d1, bmt$t2, bmt$d2, st)
#'  print(fit, digits=2)
#' }
#'
#' fit2 = tteICE(Surv.ice(t2, d4)~A, data=bmt,
#' strategy="composite", cov.formula=~z1+z3+z5, method='eff')
#' print(fit2)
#' fit3 = tteICE(Surv.ice(t1, d1, t2, d2)~A, data=bmt,
#' strategy="composite", cov.formula=~z1+z3+z5, method='eff')
#' print(fit3)
#'
#' @seealso
#' \code{\link[tteICE]{surv.tteICE}},
#' \code{\link[tteICE]{scr.tteICE}}
#'
#' @method print tteICE
#' @return Print the summary of a tteICE object
#' @export

print.tteICE <- function(x, digits=4, ...){
  # if (!inherits(x, "tteICE"))  stop("Must be an object returned by `surv.tteICE` or `scr.tteICE`.", call. = FALSE)
  if(is.null(x$p.val)) p=NA else p=x$p.val
  dtype = c(cmprsk="competing risks", smcmprsk="semicompeting risks")
  strat = c(treatment="treatment policy strategy", composite="composite variable strategy",
           natural="hypothetical strategy (controlling the hazard of ICEs)",
           removed="hypothetical strategy (removing ICEs)",
           whileon="while on treatment strategy", principal="principal stratum strategy")
  meth = c(np="nonparametric estimation", eff="semiparametrically efficient estimation",
           ipw="inverse probability weighting")
  cat("Data type:", dtype[x$dtype], "\n")
  cat("Strategy:", strat[x$strategy], "\n")
  cat("Estimation method:", meth[x$method], "\n")
  cat("Observations:", x$n, '(including', x$n1, 'treated and', x$n0, 'control)\n')
  cat("Maximum follow-up time:", max(x$time), '\n')
  cat("P-value of the average treatment effect:", round(p, digits), "\n")
  cat("-----------------------------------------------------------------------\n")
  cat("The estimated cumulative incidences and treatment effects at quartiles:\n")
  print(round(riskpredict(x), digits))
  cat("\n")
  invisible(x)
}
