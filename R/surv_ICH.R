#' @title Fitting the cumulative incidence function for time-to event data under ICH E9 (R1)
#'
#' @description This function estimates the potential cumulative incidence function
#' for time-to event data under ICH E9 (R1) to address intercurrent events. Multiple
#' strategies are allowed. The input data should be of competing risks structure.
#'
#' @param A Treatment indicator, 1 for treatment and 0 for control.
#'
#' @param Time Time to event.
#'
#' @param cstatus Indicator of event, 1 for the primary event, 2 for the intercurrent event, 0 for censoring.
#'
#' @param strategy Stragety to address intercurrent events, \code{"treatment"} indicating treatment policy strategy,
#' \code{"composite"} indicating composite variable strategy, \code{"natural"} indicating hypothetical strategy
#' (Scenario I, controlling the hazard of intercurrent events), \code{"removed"} indicating hypothetical strategy
#' (Scenario II, removing intercurrent events), \code{"whileon"} indicating while on treatment strategy, and
#' \code{"principal"} indicating principal stratum strategy.
#'
#' @param cov1 Baseline covariates.
#'
#' @param method Estimation method, \code{"np"} indicating nonparametric estimation, otherwise
#' indicating semiparametrically efficient estimation based on efficient influence functions.
#'
#' @param weights Weight for each subject.
#'
#' @param subset Subset, either numerical or logical.
#'
#'
#' @return A list including the fitted object and input variables.
#'
#' @examples
#' ## Generate simulated data
#' ## dat = .generatedata(500)
#' ## composite variable strategy, 
#' ## nonparametric estimation without covariates
#' fit1 = surv.ICH(dat$Z, dat$Time, dat$cstatus, "composite")
#' ## hypothetical strategy (natural effects),
#' ## nonparametric estimation with inverse probability weighting
#' ps = predict(glm(dat$Z ~ dat$X, family='binomial'), type='response')
#' w = dat$Z/ps + (1-dat$Z)/(1-ps)
#' fit1 = surv.ICH(dat$Z, dat$Time, dat$cstatus, "natural", dat$X, weights=w)
#' ## composite variable strategy, semiparametrically efficient estimation with covariates
#' fit2 = surv.ICH(dat$Z, dat$Time, dat$cstatus, "composite", dat$X, method='eff')
#' 
#'
#' @details
#' \describe{
#' Intercurrent events refer to the events occurring after treatment initiation of clinical trials that
#' affect either the interpretation of or the existence of the measurements associated with the clinical
#' question of interest. The International Conference on Harmonization (ICH) E9 (R1) addendum proposed
#' five strategies have been proposed in to address intercurrent events, namely, treatment policy strategy,
#' composite variable strategy, while on treatment strategy, hypothetical strategy, and principal stratum
#' strategy. To answer a specific scientific question, a strategy with a particular estimand is chosen
#' before the study design. \\cr
#' We adopt the potential outcomes framework that defines a causal estimand as the contrast between
#' functionals of potential outcomes. Consider a randomized controlled trial with \eqn{n} individuals
#' randomly assigned to one of two treatment conditions, denoted by \eqn{w}, where \eqn{w = 1} represents
#' the active treatment (a test drug) and \eqn{w = 0} represents the control (placebo). Assume that all
#' patients adhere to their treatment assignments and do not discontinue treatment. Associated with individual
#' \eqn{i = 1, ..., n} are two potential time-to-event primary outcomes \eqn{T_i(1)} and \eqn{T_i(0)},
#' if any, which represent the time durations from treatment initiation to the primary outcome event under
#' two treatment assignments respectively. Let \eqn{R_i(1)} and \eqn{R_i(0)} denote the occurrence time of
#' potential intercurrent events, if any, under the two treatment assignments, respectively. Intercurrent
#' events are considered as absent if no post-treatment intercurrent events occur until the end of study. \\cr
#' We adopt the potential cumulative incidences under both treatment assignments as the target estimands.
#' Potential cumulative incidences describe the probability of time-to-event outcomes occurring at each
#' time point. We define the treatment effect as the contrast of two potential cumulative incidences.
#' Cumulative incidences are model-free and collapsible, enjoying causal interpretations.
#' }
#'
#' @seealso \code{\link[ICHe9r1]{surv.boot}}
#'
#'
#' @export


surv.ICH <- function(A,Time,cstatus,strategy='composite',cov1=NULL,method='np',
                     weights=NULL,subset=NULL){
  N = length(A)
  if (is.null(weights)) weights = rep(1,N)
  #if (!is.null(cov1)) weights = weights*.ipscore(A,cov1)
  if (method=='np'){
    if (strategy=='treatment') fit = surv.treatment(A,Time,cstatus,weights,subset)
    if (strategy=='composite') fit = surv.composite(A,Time,cstatus,weights,subset)
    if (strategy=='natural') fit = surv.natural(A,Time,cstatus,weights,subset)
    if (strategy=='removed') fit = surv.removed(A,Time,cstatus,weights,subset)
    if (strategy=='whileon') fit = surv.whileon(A,Time,cstatus,weights,subset)
    if (strategy=='principal') fit = surv.principal(A,Time,cstatus,weights,subset)
  } else {
    if (strategy=='treatment') fit = surv.treatment.eff(A,Time,cstatus,cov1,subset)
    if (strategy=='composite') fit = surv.composite.eff(A,Time,cstatus,cov1,subset)
    if (strategy=='natural') fit = surv.natural.eff(A,Time,cstatus,cov1,subset)
    if (strategy=='removed') fit = surv.removed.eff(A,Time,cstatus,cov1,subset)
    if (strategy=='whileon') fit = surv.whileon.eff(A,Time,cstatus,cov1,subset)
    if (strategy=='principal') fit = surv.principal.eff(A,Time,cstatus,cov1,subset)
  }
  ate.list = c(fit,list(A=A,Time=Time,cstatus=cstatus,strategy=strategy,cov1=cov1,
                    method=method,weights=weights,subset=subset,dtype='cmprsk'))

  class(ate.list)="ICH"
  return(ate.list)

}
