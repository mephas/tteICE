#' @title Summary of tteICE
#'
#' @description This function summarizes the results
#'
#' @param object A fitted object returned by the function \code{surv.tteICE} or \code{scr.tteICE}.
#'
#' @param ... Other augments in function \code{\link{summary}}
#'
#' @importFrom stats quantile
#'
#' @examples
#' ## load data
#' data(bmt)
#' bmt = transform(bmt, d4=d2+d3)
#' A = as.numeric(bmt$group>1)
#' ## summarize the results
#' fit1 = surv.tteICE(A, bmt$t2, bmt$d4, 'natural')
#' summary(fit1)
#' fit2 = tteICE(Surv.ice(t2, d4)~A, data=bmt,
#' strategy="composite", cov.formula=~z1+z3+z5, method='eff')
#' summary(fit2)
#' fit3 = tteICE(Surv.ice(t1, d1, t2, d2)~A, data=bmt,
#' strategy="composite", cov.formula=~z1+z3+z5, method='eff')
#' summary(fit3)
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

  res = list(dtype=object$dtype, strategy=object$strategy, method=object$method, maxt=max(object$time),
             n=object$n, n1=object$n1, n0=object$n0, p.val=object$p.val, est=riskpredict(object))
  class(res) <- "summary.tteICE"
  res
}
