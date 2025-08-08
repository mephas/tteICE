#' @title Fitting the cumulative incidence function using hypothetical strategy (I)
#'
#' @description This function estimates the potential cumulative incidence function
#' based on efficient influence functions using hypothetical strategy (semicompeting risks
#' data structure). Cox models are employed for survival models. The intercurrent event
#' is only permitted under treated if is would occur under control.
#'
#' @param A Treatment indicator, 1 for treatment and 0 for control.
#'
#' @param Time Time to the primary (terminal) event.
#'
#' @param status Indicator of the primary (terminal) event, 1 for event and 0 for censoring.
#'
#' @param Time_int Time to the intercurrent event.
#'
#' @param status_int Indicator of the intercurrent event, 1 for event and 0 for censoring.
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
#' @examples
#' 1
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
#' in the pre-specified hypothetical scenario under active treatment and placebo. \\cr
#' The key question is how to envision \eqn{T'(w)}. We manipulate the hazard specific to intercurrent
#' event \eqn{\lambda_2(t; w)} while assuming the hazard specific to the primary outcome event
#' \eqn{\lambda_1(t; w)} remains unchanged. Specifically, we envision that the intercurrent events that
#' occurred when individuals were assigned to test drugs were only permitted if these intercurrent events
#' would have also occurred if these individuals had been assigned to the placebo. In this hypothetical
#' scenario, when assigned to placebo, individuals would be equally likely to experience intercurrent
#' events as they are assigned to placebo in the real-world trial in terms of the hazards; when assigned
#' to test drug, the hazard of intercurrent events would be identical to that if assigned to placebo in
#' the real-world trial. That is, \eqn{\lambda_2'(t;0) = \lambda_2'(t;1) = \lambda_2(t;0)}. The treatment
#' effect corresponds to the natural direct effect with the hazard of intercurrent events set at
#' the level under control.
#' }
#'
#' @seealso \code{\link[ICHe9r1]{scr.natural}}, \code{\link[ICHe9r1]{scr.ICH}}
#'
#'
#' @export
scr.natural.eff <- function(A,Time,status,Time_int,status_int,X=NULL,subset=NULL){
  Td = Time; Dd = status
  Tr = Time_int; Dr = status_int
  if (!is.null(subset)) {
    A = A[subset]
    Td = Td[subset]; Dd = Dd[subset]; Tr = Td[subset]; Dr = Dr[subset]
    if (!is.null(X)) X = as.matrix(X)[subset,]
  }
  n = length(A)
  if (!is.null(X)) X = as.matrix(X)
  tt = sort(unique(c(Tr,Td)))
  l = length(tt)
  # hazard of d
  fit_d = .phfit_d(Tr,Dr,Td,Dd,A,X,a=1)
  Xb = fit_d$Xb
  delta_r = fit_d$delta_r
  lam_d = .matchy(fit_d$lam, fit_d$tt, tt, TRUE)
  lam_od1 = sapply(1:l, function(t) lam_d[t]*exp(Xb))
  lam_ord1 = lam_od1 * exp(delta_r)
  fit_d = .phfit_d(Tr,Dr,Td,Dd,A,X,a=0)
  Xb = fit_d$Xb
  delta_r = fit_d$delta_r
  lam_d = .matchy(fit_d$lam, fit_d$tt, tt, TRUE)
  lam_od0 = sapply(1:l, function(t) lam_d[t]*exp(Xb))
  lam_ord0 = lam_od0 * exp(delta_r)
  # hazard of r
  fit_r = .phfit_r(Tr,Dr,Td,Dd,A,X,a=1)
  Xb = fit_r$Xb
  lam_r = .matchy(fit_r$lam, fit_r$tt, tt, TRUE)
  lam_or1 = sapply(1:l, function(t) lam_r[t]*exp(Xb))
  fit_r = .phfit_r(Tr,Dr,Td,Dd,A,X,a=0)
  Xb = fit_r$Xb
  lam_r = .matchy(fit_r$lam, fit_r$tt, tt, TRUE)
  lam_or0 = sapply(1:l, function(t) lam_r[t]*exp(Xb))
  # hazard of c
  fit_c = .phfit_c(Tr,Dr,Td,Dd,A,X,a=1)
  Xb = fit_c$Xb
  lam_c = .matchy(fit_c$lam, fit_c$tt, tt, TRUE)
  lam_c1 = sapply(1:l, function(t) lam_c[t]*exp(Xb))
  fit_c = .phfit_c(Tr,Dr,Td,Dd,A,X,a=0)
  Xb = fit_c$Xb
  lam_c = .matchy(fit_c$lam, fit_c$tt, tt, TRUE)
  lam_c0 = sapply(1:l, function(t) lam_c[t]*exp(Xb))

  if (!is.null(X)){
    fit = glm(A~X, family='binomial')
  } else {
    fit = glm(A~1, family='binomial')
  }
  ps = matrix(1,nrow=n,ncol=2)
  pscore = predict(fit, type='response')
  ps[,1] = (1 - pscore) * mean((1-A)/(1-pscore))
  ps[,2] = pscore * mean(A/pscore)

  # observable incidence
  lam_od_A = A*lam_od1 + (1-A)*lam_od0
  lam_or_A = A*lam_or1 + (1-A)*lam_or0
  lam_ord_A = A*lam_ord1 + (1-A)*lam_ord0
  Lam_o_A = t(apply(lam_od_A+lam_or_A, 1, cumsum))
  Lam_or_A = t(apply(lam_ord_A, 1, cumsum))
  lam_c_A = A*lam_c1 + (1-A)*lam_c0
  Lam_c_A = t(apply(lam_c_A, 1, cumsum))
  SC = 1 - t(apply(exp(-Lam_c_A)*lam_c_A, 1, cumsum))

  dF_or_A = exp(-Lam_o_A)*lam_or_A
  dF_od_A = exp(-Lam_o_A)*lam_od_A
  F_or_A = t(apply(dF_or_A, 1, cumsum))
  F_od_A = t(apply(dF_od_A, 1, cumsum))
  dF_or__A = t(apply(dF_or_A*exp(Lam_or_A), 1, cumsum))*exp(-Lam_or_A)
  dF_ord_A = dF_or__A*lam_ord_A
  F_ord_A = t(apply(dF_ord_A, 1, cumsum))
  F_A = colMeans(F_od_A+F_ord_A)

  a = c(1,0,1)
  lam_od_a = a[1]*lam_od1 + (1-a[1])*lam_od0
  lam_or_a = a[2]*lam_or1 + (1-a[2])*lam_or0
  lam_ord_a = a[3]*lam_ord1 + (1-a[3])*lam_ord0
  Lam_o_a = t(apply(lam_od_a+lam_or_a, 1, cumsum))
  Lam_or_a = t(apply(lam_ord_a, 1, cumsum))

  dF_or_a = exp(-Lam_o_a)*lam_or_a
  dF_od_a = exp(-Lam_o_a)*lam_od_a
  F_or_a = t(apply(dF_or_a, 1, cumsum))
  F_od_a = t(apply(dF_od_a, 1, cumsum))
  dF_or__a = t(apply(dF_or_a*exp(Lam_or_a), 1, cumsum))*exp(-Lam_or_a)
  dF_ord_a = dF_or__a*lam_ord_a
  F_ord_a = t(apply(dF_ord_a, 1, cumsum))
  Fd = F_od_a + F_ord_a

  # od
  Y_o = sapply(tt, function(t) (Td>=t)*(Tr>=t))
  PY_o = (1-F_or_A-F_od_A) * SC
  dM_od = sapply(tt, function(t) (Td==t)*Dd*(Tr>=t)) - Y_o*lam_od_A
  dQ_od = dM_od * (A==a[1])/ps[,a[1]+1] /PY_o
  dM_or = sapply(tt, function(t) (Tr==t)*Dr*(Td>=t)) - Y_o*lam_or_A
  dQ_or = dM_or * (A==a[2])/ps[,a[2]+1] / PY_o
  dQ_od[PY_o==0] = 0
  dQ_or[PY_o==0] = 0
  Q_or = t(apply(dQ_or,1,cumsum))
  Q_od = t(apply(dQ_od,1,cumsum))
  G1_od = dQ_od - (Q_od+Q_or)*lam_od_a
  G_od = t(apply(G1_od*exp(-Lam_o_a), 1, cumsum))
  # ord
  Y_or = sapply(tt, function(t) (Td>=t)*(Tr<=t)*Dr)
  PY_or = (F_or_A - F_ord_A) * SC
  dM_ord = sapply(tt, function(t) (Td==t)*Dd*Dr*(Tr<=t)) - Y_or*lam_ord_A
  dQ_ord = dM_ord * (A==a[3])/ps[,a[3]+1] / PY_or
  dQ_ord[PY_or==0] = 0
  Q_ord = t(apply(dQ_ord,1,cumsum))
  G1_or = dQ_or - (Q_od+Q_or)*lam_or_a
  G1_or = t(apply(G1_or*exp(Lam_or_a-Lam_o_a), 1, cumsum))
  G2_or = dQ_ord - Q_ord*lam_ord_a
  G2_or1 = t(apply(exp(Lam_or_a-Lam_o_a)*lam_or_a, 1, cumsum))
  G2_or2 = G2_or1 * G2_or
  G3_or = t(apply(exp(Lam_or_a-Lam_o_a)*lam_or_a*Q_ord, 1, cumsum))
  G_ord = t(apply(((G1_or+G3_or)*lam_ord_a+G2_or2)*exp(-Lam_or_a), 1, cumsum))

  EIF1 = G_od + G_ord
  Feff1 = Fd + EIF1

  a = c(0,0,0)
  lam_od_a = a[1]*lam_od1 + (1-a[1])*lam_od0
  lam_or_a = a[2]*lam_or1 + (1-a[2])*lam_or0
  lam_ord_a = a[3]*lam_ord1 + (1-a[3])*lam_ord0
  Lam_o_a = t(apply(lam_od_a+lam_or_a, 1, cumsum))
  Lam_or_a = t(apply(lam_ord_a, 1, cumsum))

  dF_or_a = exp(-Lam_o_a)*lam_or_a
  dF_od_a = exp(-Lam_o_a)*lam_od_a
  F_or_a = t(apply(dF_or_a, 1, cumsum))
  F_od_a = t(apply(dF_od_a, 1, cumsum))
  dF_or__a = t(apply(dF_or_a*exp(Lam_or_a), 1, cumsum))*exp(-Lam_or_a)
  dF_ord_a = dF_or__a*lam_ord_a
  F_ord_a = t(apply(dF_ord_a, 1, cumsum))
  Fd = F_od_a + F_ord_a

  # od
  Y_o = sapply(tt, function(t) (Td>=t)*(Tr>=t))
  PY_o = (1-F_or_A-F_od_A) * SC
  dM_od = sapply(tt, function(t) (Td==t)*Dd*(Tr>=t)) - Y_o*lam_od_A
  dQ_od = dM_od * (A==a[1])/ps[,a[1]+1] /PY_o
  dM_or = sapply(tt, function(t) (Tr==t)*Dr*(Td>=t)) - Y_o*lam_or_A
  dQ_or = dM_or * (A==a[2])/ps[,a[2]+1] / PY_o
  dQ_od[PY_o==0] = 0
  dQ_or[PY_o==0] = 0
  Q_or = t(apply(dQ_or,1,cumsum))
  Q_od = t(apply(dQ_od,1,cumsum))
  G1_od = dQ_od - (Q_od+Q_or)*lam_od_a
  G_od = t(apply(G1_od*exp(-Lam_o_a), 1, cumsum))
  # ord
  Y_or = sapply(tt, function(t) (Td>=t)*(Tr<=t)*Dr)
  PY_or = (F_or_A - F_ord_A) * SC
  dM_ord = sapply(tt, function(t) (Td==t)*Dd*Dr*(Tr<=t)) - Y_or*lam_ord_A
  dQ_ord = dM_ord * (A==a[3])/ps[,a[3]+1] / PY_or
  dQ_ord[PY_or==0] = 0
  Q_ord = t(apply(dQ_ord,1,cumsum))
  G1_or = dQ_or - (Q_od+Q_or)*lam_or_a
  G1_or = t(apply(G1_or*exp(Lam_or_a-Lam_o_a), 1, cumsum))
  G2_or = dQ_ord - Q_ord*lam_ord_a
  G2_or1 = t(apply(exp(Lam_or_a-Lam_o_a)*lam_or_a, 1, cumsum))
  G2_or2 = G2_or1 * G2_or
  G3_or = t(apply(exp(Lam_or_a-Lam_o_a)*lam_or_a*Q_ord, 1, cumsum))
  G_ord = t(apply(((G1_or+G3_or)*lam_ord_a+G2_or2)*exp(-Lam_or_a), 1, cumsum))

  EIF0 = G_od + G_ord
  Feff0 = Fd + EIF0

  cif1 = apply(Feff1, 2, mean, na.rm=TRUE)
  cif0 = apply(Feff0, 2, mean, na.rm=TRUE)
  se1 = apply(Feff1, 2, sd, na.rm=TRUE) / sqrt(n)
  se0 = apply(Feff0, 2, sd, na.rm=TRUE) / sqrt(n)
  ate = cif1 - cif0
  se = apply(Feff1-Feff0, 2, sd, na.rm=TRUE) / sqrt(n)
  Ti = (tt<=0.99*max(tt))
  Tt = sum((cif1-cif0)*diff(c(0,tt))*Ti)
  IFt = colSums(t(EIF1-EIF0)*diff(c(0,tt))*Ti)
  Vt = sd(IFt, na.rm=TRUE)/sqrt(n)
  p = 2*pnorm(-abs(Tt/Vt))
  if (tt[1]>0) {
    tt = c(0,tt); cif1 = c(0,cif1); cif0 = c(0,cif0)
    se1 = c(0,se1); se0 = c(0,se0); ate = c(0,ate); se = c(0,se)
  }

  return(list(time1=tt,time0=tt,cif1=cif1,cif0=cif0,se1=se1,se0=se0,
              time=tt,ate=ate,se=se,p.val=p))
}
