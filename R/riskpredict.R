#' @title Risk prediction at specific time points
#'
#' @description This function predicts the potential cumulative incidence function and treatment effect at
#' specific time points.
#'
#' @param fit A fitted object returned by the function \code{surv.tteICE} or \code{scr.tteICE}.
#'
#' @param timeset Time at which to predict the risk. If \code{timeset=NULL}, risks will be predict at the
#' quartiles of the maximum follow-up time.
#'
#' @return A matrix. The meanings of each row are: time points, potential cumulative incidences (under
#' treated and under control), treatment effects, standard errors, and P-values.
#'
#'
#' @seealso \code{\link[tteICE]{scr.tteICE}}, \code{\link[tteICE]{surv.tteICE}}, \code{\link[tteICE]{surv.boot}}
#'
#' @examples
#' ## load data
#' data(bmt)
#' bmt = transform(bmt, d4=d2+d3)
#' A = as.numeric(bmt$group>1)
#' X = as.matrix(bmt[,c('z1','z3','z5')])
#' ## Composite variable strategy,
#' ## nonparametric estimation without covariates
#' fit1 = scr.tteICE(A, bmt$t1, bmt$d1, bmt$t2, bmt$d2, "composite")
#' riskpredict(fit1, timeset=c(670,2000))
#'
#' @export

riskpredict <- function(fit, timeset=NULL){

  .riskpredict_validate(fit, timeset)
  if (is.null(timeset)) {
    maxt = max(fit$time)
    timeset = c(0.25,0.5,0.75,1)*maxt
  }
  cif1 = .matchy(fit$cif1,fit$time,timeset)
  cif0 = .matchy(fit$cif0,fit$time,timeset)
  ate = cif1 - cif0
  se1 = .matchy(fit$se1,fit$time,timeset)
  se0 = .matchy(fit$se0,fit$time,timeset)
  se = .matchy(fit$se,fit$time,timeset)
  p = 2*pnorm(-abs(ate/se))
  res = rbind(cif1,se1,cif0,se0,ate,se,p)
  colnames(res) = timeset
  rownames(res) = c('CIF1','se1','CIF0','se0','ATE','se','p.val')
  return(res)
}
