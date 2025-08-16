#' @title Risk prediction at specific time points
#'
#' @description This function predicts the potential cumulative incidence function and treatment effect at
#' specific time points.
#'
#' @param fit An ICH object.
#'
#' @param timeset Time at which to predict the risk. If \code{timeset=NULL}, risks will be predict at the 
#' quartiles of the maximum follow-up time.
#'
#'
#' @return A matrix. The meanings of each row are: time points, potential cumulative incidences (under 
#' treated and under control), treatment effects, standard errors, P-values.
#' 
#'
#' @seealso \code{\link[ICHe9r1]{scr.ICH}}, \code{\link[ICHe9r1]{surv.ICH}}, \code{\link[ICHe9r1]{surv.boot}}
#'
#'
#' @export

riskpredict <- function(fit, timeset=NULL, nboot=0, seed=0){
  fit = surv.boot(fit, nboot, seed)
  if (is.null(timeset)) {
    maxt = max(fit$time)
    timeset = c(0.25,0.5,0.75,1)*maxt
  }
  cif1 = .matchy(fit$time,fit$cif1,timeset)
  cif0 = .matchy(fit$time,fit$cif0,timeset)
  ate = cif1 - cif0
  se1 = .matchy(fit$time,fit$se1,timeset)
  se0 = .matchy(fit$time,fit$se0,timeset)
  cif1 = .matchy(fit$time,fit$cif1,timeset)
  se = .matchy(fit$time,fit$se,timeset)
  p = 2*pnorm(-abs(ate/se))
  res = rbind(cif1,se1,cif0,se0,ate,se,p)
  colnames(res) = timeset
  rownames(res) = c('CIF1','se1','CIF0','se0','ATE','se','p.val')
  return(res)
}
