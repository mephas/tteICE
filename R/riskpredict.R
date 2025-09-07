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
#' @param nboot Number of resamplings in bootstrapping. Default \code{nboot = 0}, using the explicit
#' formula of the standard error.
#'
#' @param seed Seed for bootstrapping.
#' 
#' @return A matrix. The meanings of each row are: time points, potential cumulative incidences (under 
#' treated and under control), treatment effects, standard errors, and P-values.
#' 
#'
#' @seealso \code{\link[tteICE]{scr.ICH}}, \code{\link[tteICE]{surv.ICH}}, \code{\link[tteICE]{surv.boot}}
#'
#'
#' @export

riskpredict <- function(fit, timeset=NULL, nboot=0, seed=0){
  fit = surv.boot(fit, nboot, seed)
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
