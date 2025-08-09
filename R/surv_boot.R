#' @title Calculating the standard error for the estimated cumulative incidence function and treatment effect
#'
#' @description This function calculates the standard error for the estimated potential cumulative incidence function
#' and treatment effect. Two methods to calculate the standard error are considered: asymptotic standard error
#' based on the explicit formula and bootstrapping.
#'
#' @param fit A fitted object from \code{surv.ICH}.
#'
#' @param nboot Number of resamplings in the boostrapping method. If \code{nboot} is smaller than 1, then
#' asymptotic standard error based on the explicit form is calculated instead of bootstrapping.
#'
#' @param seed Seed for bootstrapping.
#'
#'
#' @return A list including
#' \describe{
#' \item{time1}{Time points in the treated group.}
#' \item{time0}{Time points in the control group.}
#' \item{cif1}{Estimated cumulative incidence function in the treated group.}
#' \item{cif0}{Estimated cumulative incidence function in the control group.}
#' \item{se1}{Standard error of the estimated cumulative incidence function in the treated group.}
#' \item{se0}{Standard error of the estimated cumulative incidence function in the control group.}
#' \item{time}{Time points in both groups.}
#' \item{ate}{Estimated treatment effect (difference in cumulative incidence functions).}
#' \item{se}{Standard error of the estimated treatment effect.}
#' \item{strategy}{Strategy used.}
#' \item{method}{Estimation method used.}
#' }
#'
#'
#' @seealso \code{\link[ICHe9r1]{surv.ICH}}, \code{\link[ICHe9r1]{scr.ICH}}
#'
#'
#' @export

surv.boot <- function(fit,nboot=0,seed=0){
  N = length(fit$A)
  time1 = fit$time1
  time0 = fit$time0
  Time = sort(unique(c(0,fit$Time[fit$cstatus>0],max(fit$Time))))
  cif1 = fit$cif1
  cif0 = fit$cif0
  se1 = fit$se1
  se0 = fit$se0
  ate = .matchy(fit$ate,fit$time,Time)
  se = .matchy(fit$se,fit$time,Time)
  if (nboot>1){
    cif1l = cif0l = te = NULL
    set.seed(seed)
    for(b in 1:nboot){
      #wt = as.vector(rmultinom(1,N,rep(1/N,N)))
      subset = sample(1:N,replace=TRUE)
      if (fit$dtype=='cmprsk'){
      fitb = surv.ICH(fit$A,fit$Time,fit$cstatus,fit$strategy,fit$cov1,fit$method,
                      fit$weights,subset)
      } else {
      fitb = scr.ICH(fit$A,fit$Time,fit$status,fit$Time_int,fit$status_int,fit$strategy,fit$cov1,fit$method,
                        fit$weights,subset)
      }
      cifb1 = .matchy(fitb$cif1,fitb$time1,Time)
      cifb0 = .matchy(fitb$cif0,fitb$time0,Time)
      cif1l = rbind(cif1l, cifb1)
      cif0l = rbind(cif0l, cifb0)
      te = rbind(te, cifb1-cifb0)
    }
    se1 = apply(cif1l,2,sd)
    se0 = apply(cif0l,2,sd)
    se1 = .matchy(se1,Time,time1)
    se0 = .matchy(se0,Time,time0)
    se = apply(te,2,sd)
  }
  return(list(time1=time1,time0=time0,cif1=fit$cif1,cif0=fit$cif0,
              se1=se1,se0=se0,time=Time,ate=ate,se=se,
              strategy=fit$strategy,method=fit$method,dtype=fit$dtype))
}

