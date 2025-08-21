#' @title Fitting the cumulative incidence function using hypothetical strategy (II)
#'
#' @description This function estimates the potential cumulative incidence function based on
#' efficient influence functions using hypothetical strategy (competing risks data structure).
#' Cox models are employedfor survival models. The intercurrent event is assumed to be absent
#' in the hypothetical scenario.
#'
#' @param A Treatment indicator, 1 for treatment and 0 for control.
#'
#' @param Time Time to event.
#'
#' @param cstatus Indicator of event, 1 for the primary event, 2 for the intercurrent event, 0 for censoring.
#'
#' @param X Baseline covariates.
#'
#' @param subset Subset, either numerical or logical.
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
#' \item{p.val}{P value of testing the treatment effect based on the efficient influence function of
#' the restricted mean survival time lost by the end of study.}
#' }
#'
#' @details
#' \describe{
#' The hypothetical strategy envisions a hypothetical clinical trial condition where the occurrence
#' of intercurrent events is restricted in certain ways. By doing so, the distribution of potential
#' outcomes under the hypothetical scenario can capture the impact of intercurrent events explicitly
#' through a pre-specified criterion. We use \eqn{T'(w)}, \eqn{w = 1, 0} to denote the time to the
#' primary outcome event in the hypothetical scenario. The time-dependent treatment effect specific
#' to this hypothetical scenario is written as
#' \eqn{\tau(t) = P(T'(1) < t) - P(T'(0) < t),}
#' representing the difference in probabilities of experiencing primary outcome events during \eqn{(0,t)}
#' in the pre-specified hypothetical scenario under active treatment and placebo. \cr
#' The key question is how to envision \eqn{T'(w)}. We manipulate the hazard specific to intercurrent
#' event \eqn{\lambda_2(t; w)} while assuming the hazard specific to the primary outcome event
#' \eqn{\lambda_1(t; w)} remains unchanged. Specifically, we envision that intercurrent events are
#' absent in the hypothetical scenario for all individuals, so \eqn{\lambda_2'(t;0) = \Lambda_2'(t;1) = 0}.
#' This hypothetical scenario leads to an estimand called the marginal cumulative incidence. The treatment
#' effect corresponds to the controlled direct effect with the intercurrent events removed.

#' }
#'
#' @seealso \code{\link[ICHe9r1]{surv.removed}}, \code{\link[ICHe9r1]{surv.ICH}}
#'
#'
#' @export

surv.removed.eff <- function(A,Time,cstatus,X=NULL,subset=NULL){
  if (is.null(subset)) subset = 1:length(A)
  n = length(A[subset])
  if (is.null(X)){
    psfit = glm(A~NULL, family='binomial', subset=subset)
    fit11 = coxph(Surv(Time,cstatus==1)~NULL, subset=subset[A[subset]==1])
    fit10 = coxph(Surv(Time,cstatus==1)~NULL, subset=subset[A[subset]==0])
    fit21 = coxph(Surv(Time,cstatus>1)~NULL, subset=subset[A[subset]==1])
    fit20 = coxph(Surv(Time,cstatus>1)~NULL, subset=subset[A[subset]==0])
    fit1c = coxph(Surv(Time,cstatus==0)~NULL, subset=subset[A[subset]==1])
    fit0c = coxph(Surv(Time,cstatus==0)~NULL, subset=subset[A[subset]==0])
  } else {
    X = as.matrix(scale(X))
    psfit = glm(A~X, family='binomial', subset=subset)
    fit11 = coxph(Surv(Time,cstatus==1)~X, subset=subset[A[subset]==1])
    fit10 = coxph(Surv(Time,cstatus==1)~X, subset=subset[A[subset]==0])
    fit21 = coxph(Surv(Time,cstatus>1)~X, subset=subset[A[subset]==1])
    fit20 = coxph(Surv(Time,cstatus>1)~X, subset=subset[A[subset]==0])
    fit1c = coxph(Surv(Time,cstatus==0)~X, subset=subset[A[subset]==1])
    fit0c = coxph(Surv(Time,cstatus==0)~X, subset=subset[A[subset]==0])
  }
  ps = predict(psfit, type='response')
  tt1 = c(0,basehaz(fit11)$time)
  tt0 = c(0,basehaz(fit10)$time)
  tt = sort(unique(c(tt1,tt0)))
  K = length(tt)
  if (!is.null(X)){
    Xb11 = as.numeric(as.matrix(X[subset,])%*%fit11$coefficients)
    Xb10 = as.numeric(as.matrix(X[subset,])%*%fit10$coefficients)
    Xb21 = as.numeric(as.matrix(X[subset,])%*%fit21$coefficients)
    Xb20 = as.numeric(as.matrix(X[subset,])%*%fit20$coefficients)
    Xb1c = as.numeric(as.matrix(X[subset,])%*%fit1c$coefficients)
    Xb0c = as.numeric(as.matrix(X[subset,])%*%fit0c$coefficients)
  } else {
    Xb11 = Xb10 = Xb21 = Xb20 = Xb1c = Xb0c = rep(0,n)
  }
  cumhaz11 = .matchy(c(0,basehaz(fit11,centered=FALSE)$hazard),tt1,tt)
  cumhaz11 = exp(Xb11)%*%t(cumhaz11)
  cumhaz10 = .matchy(c(0,basehaz(fit10,centered=FALSE)$hazard),tt0,tt)
  cumhaz10 = exp(Xb10)%*%t(cumhaz10)
  cumhaz21 = .matchy(c(0,basehaz(fit21,centered=FALSE)$hazard),c(0,basehaz(fit21)$time),tt)
  cumhaz21 = exp(Xb21)%*%t(cumhaz21)
  cumhaz20 = .matchy(c(0,basehaz(fit20,centered=FALSE)$hazard),c(0,basehaz(fit20)$time),tt)
  cumhaz20 = exp(Xb20)%*%t(cumhaz20)
  cumhaz1c = .matchy(c(0,basehaz(fit1c,centered=FALSE)$hazard),c(0,basehaz(fit1c)$time),tt)
  cumhaz0c = .matchy(c(0,basehaz(fit0c,centered=FALSE)$hazard),c(0,basehaz(fit0c)$time),tt)
  cumhaz1c = exp(Xb1c)%*%t(cumhaz1c)
  cumhaz0c = exp(Xb0c)%*%t(cumhaz0c)
  cumhaz1 = cbind(0,cumhaz11+cumhaz21)[,1:K]
  cumhaz0 = cbind(0,cumhaz10+cumhaz20)[,1:K]
  dN = sapply(tt, function(l) (Time[subset]==l)*(cstatus[subset]==1))
  Y = sapply(tt, function(l) as.numeric(Time[subset]>=l))
  lam1 = t(apply(cbind(0,cumhaz11),1,diff))
  lam0 = t(apply(cbind(0,cumhaz10),1,diff))
  dMP1 = (dN-Y*lam1)/exp(-cumhaz1-cumhaz1c)
  dMP0 = (dN-Y*lam0)/exp(-cumhaz0-cumhaz0c)
  cif1x = A[subset]/ps*exp(-cumhaz11)*t(apply(dMP1,1,cumsum))+1-exp(-cumhaz11)
  cif0x = (1-A[subset])/(1-ps)*exp(-cumhaz10)*t(apply(dMP0,1,cumsum))+1-exp(-cumhaz10)
  cif1 = colMeans(cif1x,na.rm=TRUE)
  cif0 = colMeans(cif0x,na.rm=TRUE)
  se1 = apply(cif1x,2,sd,na.rm=TRUE)/sqrt(n)
  se0 = apply(cif0x,2,sd,na.rm=TRUE)/sqrt(n)
  ate = cif1-cif0
  se = sqrt(se1^2+se0^2)
  eif1 = t(t(cif1x)-cif1)
  eif0 = t(t(cif0x)-cif0)
  Ti = (tt<0.99*max(tt))
  Tt = sum((cif1-cif0)*diff(c(0,tt))*Ti)
  IFt = colSums(t(eif1-eif0)*diff(c(0,tt))*Ti,na.rm=TRUE)
  Vt = sd(IFt,na.rm=TRUE)/sqrt(n)
  p = 2*pnorm(-abs(Tt/Vt))
  return(list(time1=tt,time0=tt,cif1=cif1,cif0=cif0,se1=se1,se0=se0,
              time=tt,ate=ate,se=se,p.val=p))
}
