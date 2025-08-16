#' @title Fitting the cumulative incidence function using treatment policy strategy
#'
#' @description This function estimates the potential cumulative incidence function based on efficient
#' influence functions using treatment policy strategy (competing risks data structure). Cox models are
#' employed for the survival model. This strategy ignores the intercurrent event and uses the time to
#' the primary event as it was recorded.
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
#' \item{tt}{Time points in both groups.}
#' \item{ate}{Estimated treatment effect (difference in cumulative incidence functions).}
#' \item{se}{Standard error of the estimated treatment effect.}
#' \item{p.val}{P value of testing the treatment effect based on the efficient influence function
#' of the restricted mean survival time lost by the end of study.}
#' }
#'
#' @details
#' \describe{
#' The treatment policy strategy addresses the problem of intercurrent events by expanding
#' the initial treatment conditions to a treatment policy. This strategy is applicable
#' only if intercurrent events do not hinder primary outcome events. The treatments under
#' comparison are now two treatment policies: \eqn{(w, R(w))}, where \eqn{w = 1, 0}. One policy
#' \eqn{(1,R(1))} involves administering the test drug, along with any naturally occurring
#' intercurrents, whereas the other policy \eqn{(0,R(0))} involves administering a placebo,
#' along with any naturally occurring intercurrents. Thus, the potential outcomes are
#' \eqn{T(1,R(1))} and \eqn{T(0,R(0))}. Instead of comparing the test drug and placebo themselves,
#' the contrast of interest is made between the two treatment policies. The difference in
#' cumulative incidences under the two treatment policies is then
#' \eqn{\tau(t) = P(T(1, R(1)) < t) - P(T(0, R(0)) < t),}{ATE_tp}
#' representing the difference in probabilities of experiencing primary outcome events during
#' \eqn{(0,t)} under active treatment and placebo. \cr
#' The average treatment effect \eqn{\tau^{\text{tp}}(t)} has a meaningful causal interpretation
#' only when \eqn{T(1, R(1))} and \eqn{T(0, R(0))} are well defined. Because the treatment policy
#' includes the occurrence of the intercurrent event as natural, the entire treatment policy is
#' determined by manipulating the initial treatment condition $w$ only. Therefore, we can simplify
#' the notations \eqn{T(w, R(w)) = T(w)} in defining estimands. As such,
#' \eqn{\tau(t) = P(T(1)) < t) - P(T(0) < t)} as the intention-to-treat analysis.
#' }
#'
#' @seealso \code{\link[ICHe9r1]{surv.treatment}}, \code{\link[ICHe9r1]{surv.ICH}}
#'
#'
#' @export

surv.treatment.eff <- function(A,Time,cstatus,X=NULL,subset=NULL){
  if (is.null(subset)) subset = 1:length(A)
  n = length(A[subset])
  if (is.null(X)){
    psfit = glm(A~NULL, family='binomial', subset=subset)
    fit1 = coxph(Surv(Time,cstatus==1)~NULL, subset=subset[A[subset]==1])
    fit0 = coxph(Surv(Time,cstatus==1)~NULL, subset=subset[A[subset]==0])
    fit1c = coxph(Surv(Time,cstatus!=1)~NULL, subset=subset[A[subset]==1])
    fit0c = coxph(Surv(Time,cstatus!=1)~NULL, subset=subset[A[subset]==0])
  } else {
    X = as.matrix(X)
    psfit = glm(A~X, family='binomial', subset=subset)
    fit1 = coxph(Surv(Time,cstatus==1)~X, subset=subset[A[subset]==1])
    fit0 = coxph(Surv(Time,cstatus==1)~X, subset=subset[A[subset]==0])
    fit1c = coxph(Surv(Time,cstatus!=1)~X, subset=subset[A[subset]==1])
    fit0c = coxph(Surv(Time,cstatus!=1)~X, subset=subset[A[subset]==0])
  }
  ps = predict(psfit, type='response')
  tt1 = c(0,basehaz(fit1)$time)
  tt0 = c(0,basehaz(fit0)$time)
  tt = sort(unique(c(tt1,tt0)))
  if (!is.null(X)){
    Xb1 = as.numeric(as.matrix(X[subset,])%*%fit1$coefficients)
    Xb0 = as.numeric(as.matrix(X[subset,])%*%fit0$coefficients)
    Xb1c = as.numeric(as.matrix(X[subset,])%*%fit1c$coefficients)
    Xb0c = as.numeric(as.matrix(X[subset,])%*%fit0c$coefficients)
  } else {
    Xb1 = Xb0 = Xb1c = Xb0c = rep(0,n)
  }
  cumhaz1 = .matchy(c(0,basehaz(fit1)$hazard),tt1,tt)
  cumhaz1 = exp(Xb1)%*%t(cumhaz1)
  cumhaz0 = .matchy(c(0,basehaz(fit0)$hazard),tt0,tt)
  cumhaz0 = exp(Xb0)%*%t(cumhaz0)
  cumhaz1c = .matchy(c(0,basehaz(fit1c)$hazard),c(0,basehaz(fit1c)$time),tt)
  cumhaz0c = .matchy(c(0,basehaz(fit0c)$hazard),c(0,basehaz(fit0c)$time),tt)
  cumhaz1c = exp(Xb1c)%*%t(cumhaz1c)
  cumhaz0c = exp(Xb0c)%*%t(cumhaz0c)
  dN = sapply(tt, function(l) (Time[subset]==l)*(cstatus[subset]==1))
  Y = sapply(tt, function(l) as.numeric(Time[subset]>=l))
  lam1 = cbind(0,t(apply(cumhaz1,1,diff)))
  dMP1 = (dN-Y*lam1)/exp(-cumhaz1-cumhaz1c)
  lam0 = cbind(0,t(apply(cumhaz0,1,diff)))
  dMP0 = (dN-Y*lam0)/exp(-cumhaz0-cumhaz0c)
  cif1x = A[subset]/ps*exp(-cumhaz1)*t(apply(dMP1,1,cumsum))+1-exp(-cumhaz1)
  cif0x = (1-A[subset])/(1-ps)*exp(-cumhaz0)*t(apply(dMP0,1,cumsum))+1-exp(-cumhaz0)
  cif1 = colMeans(cif1x)
  cif0 = colMeans(cif0x)
  se1 = apply(cif1x,2,sd)/sqrt(n)
  se0 = apply(cif0x,2,sd)/sqrt(n)
  ate = cif1-cif0
  se = sqrt(se1^2+se0^2)
  eif1 = t(t(cif1x)-cif1)
  eif0 = t(t(cif0x)-cif0)
  Ti = (tt<0.99*max(tt))
  Tt = sum((cif1-cif0)*diff(c(0,tt))*Ti)
  IFt = colSums(t(eif1-eif0)*diff(c(0,tt))*Ti)
  Vt = sd(IFt,na.rm=TRUE)/sqrt(n)
  p = 2*pnorm(-abs(Tt/Vt))
  return(list(time1=tt,time0=tt,cif1=cif1,cif0=cif0,se1=se1,se0=se0,
              time=tt,ate=ate,se=se,p.val=p))
}
