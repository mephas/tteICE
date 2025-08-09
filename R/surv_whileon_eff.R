#' @title Fitting the cumulative incidence function using while on treatment strategy
#'
#' @description This function estimates the potential cumulative incidence function
#' based on efficient influence functions using while on treatment strategy (competing
#' risks data structure). Cox models are employed for survival models. This strategy
#' can be understood as the competing risks model, which gives the subdistribution of
#' the primary event.
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
#' \item{p.val}{P value of testing the treatment effect based on the efficient influence function of
#' the restricted mean survival time lost by the end of study.}
#' }
#'
#' @details
#' \describe{
#' The while on treatment strategy considers the measure of outcome variables taken only up to
#' the occurrence of intercurrent events. The failures of primary outcome events should not be
#' counted in the cumulative incidences if intercurrent events occurred. The difference in
#' counterfactual cumulative incidences under this strategy is
#' \eqn{\tau(t) = P(T(1) < t, R(1) \geq t) - P(T(0) < t, R(0) \geq t),}
#' representing the difference in probabilities of experiencing primary outcome events without
#' intercurrent events during \eqn{(0,t)} under active treatment and placebo. The cumulative
#' incidence function is also known as the cause-specific cumulative incidence or subdistribution
#' function. \\cr
#' The while on treatment strategy is closely related to the competing risks model. However,
#' for causal interpretations, it is worth emphasizing that the hazard of \eqn{R(1)} may differ
#' from that of \eqn{R(0)}, leading to vast difference in the underlying features of individuals
#' who have not experienced the primary outcome event between treatment conditions until any time
#' \eqn{t \in (0,t^*)}, where \eqn{t^*} is the end of study. When the scientific question of
#' interest is the impact of treatment on the primary outcome event, the estimand \eqn{\tau(t)}
#' is hard to interpret if systematic difference in the risks of intercurrent events between two
#' treatment conditions under comparison is anticipated.
#' }
#'
#' @seealso \code{\link[ICHe9r1]{surv.whileon}}, \code{\link[ICHe9r1]{surv.ICH}}
#'
#'
#' @export

surv.whileon.eff <- function(A,Time,cstatus,X=NULL,subset=NULL){
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
    psfit = glm(A~X, family='binomial', subset=subset)
    fit11 = coxph(Surv(Time,cstatus==1)~X, subset=subset[A[subset]==1])
    fit10 = coxph(Surv(Time,cstatus==1)~X, subset=subset[A[subset]==0])
    fit21 = coxph(Surv(Time,cstatus>1)~X, subset=subset[A[subset]==1])
    fit20 = coxph(Surv(Time,cstatus>1)~X, subset=subset[A[subset]==0])
    fit1c = coxph(Surv(Time,cstatus==0)~X, subset=subset[A[subset]==1])
    fit0c = coxph(Surv(Time,cstatus==0)~X, subset=subset[A[subset]==0])
  }
  ps = predict(psfit, type='response')
  tt11 = c(0,basehaz(fit11)$time)
  tt10 = c(0,basehaz(fit10)$time)
  tt21 = c(0,basehaz(fit21)$time)
  tt20 = c(0,basehaz(fit20)$time)
  tt = sort(unique(c(tt11,tt10,tt21,tt20)))
  if (!is.null(X)){
    X = as.matrix(X)
    Xb11 = as.numeric(X[subset,]%*%fit11$coefficients)
    Xb10 = as.numeric(X[subset,]%*%fit10$coefficients)
    Xb21 = as.numeric(X[subset,]%*%fit21$coefficients)
    Xb20 = as.numeric(X[subset,]%*%fit20$coefficients)
    Xb1c = as.numeric(X[subset,]%*%fit1c$coefficients)
    Xb0c = as.numeric(X[subset,]%*%fit0c$coefficients)
  } else {
    Xb11 = Xb10 = Xb21 = Xb20 = Xb1c = Xb0c = rep(0,n)
  }
  cumhaz11 = .matchy(c(0,basehaz(fit11)$hazard),tt11,tt)
  cumhaz11 = exp(Xb11)%*%t(cumhaz11)
  cumhaz10 = .matchy(c(0,basehaz(fit10)$hazard),tt10,tt)
  cumhaz10 = exp(Xb10)%*%t(cumhaz10)
  cumhaz21 = .matchy(c(0,basehaz(fit21)$hazard),tt21,tt)
  cumhaz21 = exp(Xb21)%*%t(cumhaz21)
  cumhaz20 = .matchy(c(0,basehaz(fit20)$hazard),tt20,tt)
  cumhaz20 = exp(Xb20)%*%t(cumhaz20)
  cumhaz1c = .matchy(c(0,basehaz(fit1c)$hazard),c(0,basehaz(fit1c)$time),tt)
  cumhaz0c = .matchy(c(0,basehaz(fit0c)$hazard),c(0,basehaz(fit0c)$time),tt)
  cumhaz1c = exp(Xb1c)%*%t(cumhaz1c)
  cumhaz0c = exp(Xb0c)%*%t(cumhaz0c)
  cumhaz1 = cumhaz11+cumhaz21
  cumhaz0 = cumhaz10+cumhaz20
  dN1 = sapply(tt, function(l) (Time[subset]==l)*(cstatus[subset]==1))
  dN2 = sapply(tt, function(l) (Time[subset]==l)*(cstatus[subset]>1))
  Y = sapply(tt, function(l) as.numeric(Time[subset]>=l))
  lam11 = cbind(0,t(apply(cumhaz11,1,diff)))
  lam10 = cbind(0,t(apply(cumhaz10,1,diff)))
  lam21 = cbind(0,t(apply(cumhaz21,1,diff)))
  lam20 = cbind(0,t(apply(cumhaz20,1,diff)))
  dMP11 = (dN1-Y*lam11)/exp(-cumhaz1-cumhaz1c)
  dMP21 = (dN2-Y*lam21)/exp(-cumhaz1-cumhaz1c)
  dMP10 = (dN1-Y*lam10)/exp(-cumhaz0-cumhaz0c)
  dMP20 = (dN2-Y*lam20)/exp(-cumhaz0-cumhaz0c)
  cif1 = t(apply(exp(-cumhaz1)*t(apply(cbind(0,cumhaz11),1,diff)),1,cumsum))
  cif0 = t(apply(exp(-cumhaz0)*t(apply(cbind(0,cumhaz10),1,diff)),1,cumsum))
  cif1x = A[subset]/ps*t(apply(exp(-cumhaz1)*dMP11,1,cumsum))-
    A[subset]/ps*cif1*t(apply(dMP11+dMP21,1,cumsum))+
    A[subset]/ps*t(apply(cif1*(dMP11+dMP21),1,cumsum))+cif1
  cif0x = (1-A[subset])/(1-ps)*t(apply(exp(-cumhaz0)*dMP10,1,cumsum))-
    (1-A[subset])/(1-ps)*cif0*t(apply(dMP10+dMP20,1,cumsum))+
    (1-A[subset])/(1-ps)*t(apply(cif0*(dMP10+dMP20),1,cumsum))+cif0
  cif1 = colMeans(cif1x)
  cif0 = colMeans(cif0x)
  se1 = apply(cif1x,2,sd)/sqrt(n)
  se0 = apply(cif0x,2,sd)/sqrt(n)
  ate = cif1-cif0
  se = apply(cif1x-cif0x,2,sd)/sqrt(n)
  eif1 = t(t(cif1x)-cif1)
  eif0 = t(t(cif0x)-cif0)
  Tt = sum(((cif1-cif0)*diff(c(0,cif1+cif0)))[tt<0.99*max(tt)])
  V1 = apply((t(eif1-eif0)*diff(c(0,cif1+cif0)))[tt<0.99*max(tt),],2,sum)
  V2 = apply(((cif1-cif0)*apply(cbind(0,eif1+eif0),1,diff))[tt<0.99*max(tt),],2,sum)
  p = 2*pnorm(-abs(Tt)/sd(V1+V2)*sqrt(n))
  return(list(time1=tt,time0=tt,cif1=cif1,cif0=cif0,se1=se1,se0=se0,
              time=tt,ate=ate,se=se,p.val=p))
}
