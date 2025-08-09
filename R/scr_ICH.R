#' @title Fitting the cumulative incidence function for time-to event data under ICH E9 (R1)
#'
#' @description This function estimates the potential cumulative incidence function
#' for time-to event data under ICH E9 (R1) to address intercurrent events. Multiple
#' strategies are allowed. The input data should be of semicompeting risks structure.
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
#' @import survival
#' @importFrom stats rbinom rweibull rexp runif pchisq sd pnorm na.omit glm predict
#' @importFrom MASS ginv
#' @importFrom cmprsk crr
#'
#' @return A list including the fitted object and input variables.
#'
#' @examples
#' data(bmt)
#' bmt = transform(bmt, d4=d2+d3)
#' A = as.numeric(bmt$group>1)
#' X = bmt[,c('z1','z3','z5')]
#' 
#' ## composite variable strategy, 
#' ## nonparametric estimation without covariates
#' fit1 = surv.ICH(A, dat$t2, dat$d4,"composite")
#' 
#' ## hypothetical strategy (natural effects), 
#' ## nonparametric estimation with inverse probability weighting
#' ps = predict(glm(A ~ X, family='binomial'), type='response')
#' w = dat$Z/ps + (1-dat$Z)/(1-ps)
#' fit1 = surv.ICH(dA, dat$t2, dat$d3, "natural", X, weights=w)
#' 
#' ## composite variable strategy, 
#' ## semiparametrically efficient estimation with covariates
#' fit2 = scr.ICH(A, bmt$t1, bmt$d1, bmt$t2, bmt$d2, "composite", X, method='eff')
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


scr.ICH <- function(A,Time,status,Time_int,status_int,strategy='composite',cov1=NULL,method='np',
                     weights=NULL,subset=NULL){
  N = length(A)
  if (is.null(weights)) weights = rep(1,N)
  #if (!is.null(cov1)) weights = weights*.ipscore(A,cov1)
  if (method=='np'){
    if (strategy=='treatment') fit = scr.treatment(A,Time,status,Time_int,status_int,weights,subset)
    if (strategy=='composite') fit = scr.composite(A,Time,status,Time_int,status_int,weights,subset)
    if (strategy=='natural') fit = scr.natural(A,Time,status,Time_int,status_int,weights,subset)
    if (strategy=='removed') fit = scr.removed(A,Time,status,Time_int,status_int,weights,subset)
    if (strategy=='whileon') fit = scr.whileon(A,Time,status,Time_int,status_int,weights,subset)
    if (strategy=='principal') fit = scr.principal(A,Time,status,Time_int,status_int,weights,subset)
  } else {
    if (strategy=='treatment') fit = scr.treatment.eff(A,Time,status,Time_int,status_int,cov1,subset)
    if (strategy=='composite') fit = scr.composite.eff(A,Time,status,Time_int,status_int,cov1,subset)
    if (strategy=='natural') fit = scr.natural.eff(A,Time,status,Time_int,status_int,cov1,subset)
    if (strategy=='removed') fit = scr.removed.eff(A,Time,status,Time_int,status_int,cov1,subset)
    if (strategy=='whileon') fit = scr.whileon.eff(A,Time,status,Time_int,status_int,cov1,subset)
    if (strategy=='principal') fit = scr.principal.eff(A,Time,status,Time_int,status_int,cov1,subset)
  }
  return(c(fit,list(A=A,Time=Time,status=status,Time_int=Time_int,status_int=status_int,
                    strategy=strategy,cov1=cov1,method=method,weights=weights,subset=subset,
                    dtype='smcmprsk')))
}
