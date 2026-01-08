#' @title Summary of tteICE
#'
#' @description This function summarizes the results
#'
#' @param object A fitted object returned by the function \code{surv.tteICE} or \code{scr.tteICE}.
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
#' ## summarize the results
#' fit = surv.tteICE(A, bmt$t2, bmt$d4, 'natural')
#' summary(fit)
#'
#' @seealso
#' \code{\link[tteICE]{surv.tteICE}},
#' \code{\link[tteICE]{scr.tteICE}},
#' \code{\link[tteICE]{print.tteICE}}
#'
#' @method summary tteICE
#' @return A list that consists of summaries of a tteICE object: data type, strategy, estimation method, maximum follow-up time,
#' sample size, treated sample size, controlled sample size, p-value, and predicted risks at quartiles
#' @export

summary.tteICE <- function(object, ...) {

  fit<- object
  res = list(dtype=fit$dtype, strategy=fit$strategy, method=fit$method, maxt=max(fit$time),
             n=fit$n, n1=fit$n1, n0=fit$n0, p.val=fit$p.val, est=riskpredict(fit))
  class(res) <- "summary.tteICE"
  res
}
