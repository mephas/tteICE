#' @title Fitting the cumulative incidence function using principal stratum strategy
#'
#' @description This function estimates the potential cumulative incidence function
#' based on efficient influence functions using principal stratum strategy (competing
#' risks data structure). Cox models are employed for survival models. The estimand is defined in a subpopulation where
#' intercurrent events would never occur regardless of treatment conditions.
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
#' The principal stratum strategy aims to stratify the population into subpopulations based on the joint
#' potential occurrences of intercurrent events under the two treatment assignments \eqn{(R(1), R(0))}.
#' Suppose we are interested in a principal stratum comprised of individuals who would never experience
#' intercurrent events, regardless of which treatment they receive. This principal stratum can be indicated
#' by \eqn{\{R(1)=R(0)=\infty\}}. The treatment effect is now defined within this subpopulation,
#' \eqn{\tau(t) = P(T(1) < t \mid R(1)=R(0)=\infty) - P(T(0) < t \mid R(1)=R(0)=\infty),}
#' representing the difference in probabilities of experiencing primary outcome events during \eqn{(0,t)}
#' under active treatment and placebo in the subpopulation that will not experience intercurrent events
#' regardless of treatment during \eqn{(0,t)}. A principal ignorability assumption is made for identification.
#' }
#'
#' @seealso \code{\link[ICHe9r1]{surv.principal}}, \code{\link[ICHe9r1]{surv.ICH}}
#'
#'
#' @export

surv.principal.eff <- function(A,Time,cstatus,X=NULL,subset=NULL){
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
    X = as.matrix(X)
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
    Xb11 = as.numeric(as.matrix(X[subset,])%*%fit11$coefficients)
    Xb10 = as.numeric(as.matrix(X[subset,])%*%fit10$coefficients)
    Xb21 = as.numeric(as.matrix(X[subset,])%*%fit21$coefficients)
    Xb20 = as.numeric(as.matrix(X[subset,])%*%fit20$coefficients)
    Xb1c = as.numeric(as.matrix(X[subset,])%*%fit1c$coefficients)
    Xb0c = as.numeric(as.matrix(X[subset,])%*%fit0c$coefficients)
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
  lam11 = t(apply(rbind(0,cumhaz11),1,diff))
  lam10 = t(apply(rbind(0,cumhaz10),1,diff))
  lam21 = t(apply(rbind(0,cumhaz21),1,diff))
  lam20 = t(apply(rbind(0,cumhaz20),1,diff))
  dMP11 = (dN1-Y*lam11)/exp(-cumhaz1-cumhaz1c)
  dMP21 = (dN2-Y*lam21)/exp(-cumhaz1-cumhaz1c)
  dMP10 = (dN1-Y*lam10)/exp(-cumhaz0-cumhaz0c)
  dMP20 = (dN2-Y*lam20)/exp(-cumhaz0-cumhaz0c)
  cif11 = t(apply(exp(-cumhaz1)*t(apply(cbind(0,cumhaz11),1,diff)),1,cumsum))
  cif10 = t(apply(exp(-cumhaz0)*t(apply(cbind(0,cumhaz10),1,diff)),1,cumsum))
  cif21 = t(apply(exp(-cumhaz1)*t(apply(cbind(0,cumhaz21),1,diff)),1,cumsum))
  cif20 = t(apply(exp(-cumhaz0)*t(apply(cbind(0,cumhaz20),1,diff)),1,cumsum))
  cif1x = A[subset]/ps*t(apply(exp(-cumhaz1)*dMP11,1,cumsum))-
    A[subset]/ps*cif11*t(apply(dMP11+dMP21,1,cumsum))+
    A[subset]/ps*t(apply(cif11*(dMP11+dMP21),1,cumsum))+cif11
  cif0x = (1-A[subset])/(1-ps)*t(apply(exp(-cumhaz0)*dMP10,1,cumsum))-
    (1-A[subset])/(1-ps)*cif10*t(apply(dMP10+dMP20,1,cumsum))+
    (1-A[subset])/(1-ps)*t(apply(cif10*(dMP10+dMP20),1,cumsum))+cif10
  cif.wo1 = colMeans(cif1x)
  cif.wo0 = colMeans(cif0x)
  if.wo1 = t(t(cif1x)-cif.wo1)
  if.wo0 = t(t(cif0x)-cif.wo0)
  cif1x = A[subset]/ps*exp(-cumhaz1)*t(apply(dMP11+dMP21,1,cumsum))+1-exp(-cumhaz1)
  cif0x = (1-A[subset])/(1-ps)*exp(-cumhaz0)*t(apply(dMP10+dMP20,1,cumsum))+1-exp(-cumhaz0)
  cif.cv1 = colMeans(cif1x)
  cif.cv0 = colMeans(cif0x)
  if.cv1 = t(t(cif1x)-cif.cv1)
  if.cv0 = t(t(cif0x)-cif.cv0)
  cif1x = t((t(if.wo1)+cif.wo1)/min(1-cif.cv1+cif.wo1))+
    ((if.cv1-if.wo1)[,ncol(if.cv1)]/min(1-cif.cv1+cif.wo1)^2)%*%t(cif.wo1)
  cif0x = t((t(if.wo0)+cif.wo0)/min(1-cif.cv0+cif.wo0))+
    ((if.cv0-if.wo0)[,ncol(if.cv1)]/min(1-cif.cv0+cif.wo0)^2)%*%t(cif.wo0)
  cif1 = colMeans(cif1x)
  cif0 = colMeans(cif0x)
  se1 = apply(cif1x,2,sd)/sqrt(n)
  se0 = apply(cif0x,2,sd)/sqrt(n)
  ate = cif1-cif0
  se = apply(cif1x-cif0x,2,sd)/sqrt(n)
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



