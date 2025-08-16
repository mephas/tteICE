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
#' \item{time}{Time points in both groups.}
#' \item{cif1}{Estimated cumulative incidence function in the treated group.}
#' \item{cif0}{Estimated cumulative incidence function in the control group.}
#' \item{se1}{Standard error of the estimated cumulative incidence function in the treated group.}
#' \item{se0}{Standard error of the estimated cumulative incidence function in the control group.}
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
  time1 = fit$time1
  time0 = fit$time0
  if ((is.null(time1)&is.null(time1))) {
    time1 = time0 = fit$time
  }
  tt = sort(unique(c(0,time1,time0)))
  cif1 = .matchy(fit$cif1,fit$time1,tt)
  cif0 = .matchy(fit$cif0,fit$time0,tt)
  se1 = .matchy(fit$se1,fit$time1,tt)
  se0 = .matchy(fit$se0,fit$time0,tt)
  ate = cif1-cif0
  se = .matchy(fit$se,fit$time,tt)
  if (nboot>1){
    cif1l = cif0l = te = NULL
    set.seed(seed)
    for(b in 1:nboot){
      subset = sample(1:N,replace=TRUE)
      if (fit$dtype=='cmprsk'){
      fitb = surv.ICH(fit$A,fit$Time,fit$cstatus,fit$strategy,fit$cov1,fit$method,
                      fit$weights,subset)
      } else {
      fitb = scr.ICH(fit$A,fit$Time,fit$status,fit$Time_int,fit$status_int,fit$strategy,fit$cov1,fit$method,
                        fit$weights,subset)
      }
      cifb1 = .matchy(fitb$cif1,fitb$time1,tt)
      cifb0 = .matchy(fitb$cif0,fitb$time0,tt)
      cif1l = rbind(cif1l, cifb1)
      cif0l = rbind(cif0l, cifb0)
      te = rbind(te, cifb1-cifb0)
    }
    se1 = apply(cif1l,2,sd,na.rm=TRUE)
    se0 = apply(cif0l,2,sd,na.rm=TRUE)
    se = apply(te,2,sd)
  }
  return(list(time=tt,cif1=cif1,cif0=cif0,ate=ate,se1=se1,se0=se0,se=se,
              strategy=fit$strategy,method=fit$method,dtype=fit$dtype))
}

