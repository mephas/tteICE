#' @title Using formula to fit the CIF for time-to-event with intercurrent events
#'
#' @description This function estimates the potential cumulative incidence function
#' for time-to event data under ICH E9 (R1) to address intercurrent events. The input data
#' should be of a competing risks structure.
#'
#' @param formula An object of class "formula" (or one that can be coerced to that class):
#' a symbolic description of the model to be fitted.
#' The details of model specification are given under ‘Details’.
#'
#' @param data An optional data frame, list or environment (or object coercible by as.data.frame to a data frame)
#' containing the variables in the model.
#'
#' @param strategy Strategy to address intercurrent events, \code{"treatment"} indicating treatment policy strategy,
#' \code{"composite"} indicating composite variable strategy, \code{"natural"} indicating hypothetical strategy
#' (Scenario I, controlling the hazard of intercurrent events), \code{"removed"} indicating hypothetical strategy
#' (Scenario II, removing intercurrent events), \code{"whileon"} indicating while on treatment strategy, and
#' \code{"principal"} indicating principal stratum strategy.
#'
#' @param cov.formula Baseline covariates in an object of "formula".
#' The details of model specification are given under ‘Details’.
#'
#' @param method Estimation method, \code{"np"} indicating nonparametric estimation, \code{"np"} indicating inverse
#' treatment probability weighting, \code{"eff"} indicating semiparametrically efficient estimation based on efficient
#' influence functions.
#'
#' @param weights Weight for each subject.
#'
#' @param subset Subset, either numerical or logical.
#'
#' @param na.rm Whether to remove missing values.
#'
#' @param nboot Number of resamplings in the boostrapping method. If \code{nboot} is 0 or 1, then
#' asymptotic standard error based on the explicit form is calculated instead of bootstrapping.
#'
#' @param seed Seed for bootstrapping.
#'
#' @return A list including the fitted object and input variables.
#'
#' @examples
#' ## load data
#' data(bmt)
#' bmt = transform(bmt, d4=d2+d3)
#' A = as.numeric(bmt$group>1)
#' X = as.matrix(bmt[,c('z1','z3','z5')])
#' bmt$A = A
#' ## Composite variable strategy,
#' ## nonparametric estimation without covariates
#'
#' fit1 = tteICE(Surv.ice(t2, d4)~A, data=bmt,
#' strategy="composite", method='eff')
#' fit2 = tteICE(Surv.ice(t2, d4)~A, data=bmt,
#' strategy="composite", cov.formula=~z1+z3+z5, method='eff')
#' fit20 = tteICE(Surv.ice(t1, d1)~A, data=bmt,
#' strategy="composite", cov.formula=~z1+z3+z5, method='eff') ## survival??
#'
#' fit3 = tteICE(Surv.ice(t1,d1,t2,d2)~A, data=bmt,
#' strategy="composite", cov.formula=~z1+z3+z5, method='eff')
#'
#' fit30 = tteICE(Surv.ice(t2,d2,t1,d1)~A, data=bmt,
#' strategy="composite", cov.formula=~z1+z3+z5, method='eff') ## same results??
#'
#'
#' @details
#' \describe{
#' \item{Background}{Intercurrent events refer to the events occurring after treatment initiation of clinical trials that
#' affect either the interpretation of or the existence of the measurements associated with the clinical
#' question of interest. The International Conference on Harmonization (ICH) E9 (R1) addendum proposed
#' five strategies to address intercurrent events, namely, treatment policy strategy,
#' composite variable strategy, while on treatment strategy, hypothetical strategy, and principal stratum
#' strategy. To answer a specific scientific question, a strategy with a particular estimand is chosen
#' before the study design.}
#' \item{Model}{We adopt the potential outcomes framework that defines a causal estimand as the contrast between
#' functionals of potential outcomes. Consider a randomized controlled trial with \eqn{n} individuals
#' randomly assigned to one of two treatment conditions, denoted by \eqn{w}, where \eqn{w = 1} represents
#' the active treatment (a test drug) and \eqn{w = 0} represents the control (placebo). Assume that all
#' patients adhere to their treatment assignments and do not discontinue treatment. Associated with individual
#' \eqn{i = 1, ..., n} are two potential time-to-event primary outcomes \eqn{T_i(1)} and \eqn{T_i(0)},
#' if any, which represent the time durations from treatment initiation to the primary outcome event under
#' two treatment assignments respectively. Let \eqn{R_i(1)} and \eqn{R_i(0)} denote the occurrence time of
#' potential intercurrent events, if any, under the two treatment assignments, respectively. Intercurrent
#' events are considered as absent if no post-treatment intercurrent events occur until the end of study.}
#' \item{Estimand}{We adopt the potential cumulative incidences under both treatment assignments as the target estimands.
#' Potential cumulative incidences describe the probability of time-to-event outcomes occurring at each
#' time point. We define the treatment effect as the contrast of two potential cumulative incidences.
#' Cumulative incidences are model-free and collapsible, enjoying causal interpretations.}
#' \item{Formula specifications in the function}{
#' A typical model has the form \code{Surv(time, status, type = "mstate")~ A},
#' where \code{status=0,1,2} (1 for the primary event, 2 for the intercurrent event, and 0 for censoring),
#' \code{A} is the treatment indicator (1 for treatment and 0 for control).
#' An alternative model has the form \code{cbind(Surv(time1, status1), Surv(time2, status2))~ A},
#' where \code{Surv(time1, status1)} defines the time and status of primary (terminal) event, and
#' \code{Surv(time2, status2)} defines the time and status of intercurrent event.
#' Baseline covariates can be added by the model in the form \code{~ x1+x2}}
#' }
#'
#' @references
#' Deng, Y., Han, S., & Zhou, X. H. (2025).
#' Inference for Cumulative Incidences and Treatment Effects in Randomized Controlled Trials With Time-to-Event Outcomes Under ICH E9 (R1).
#' \emph{Statistics in Medicine}. \doi{10.1002/sim.70091}
#'
#' @seealso \code{\link[tteICE]{surv.boot}}, \code{\link[tteICE]{scr.tteICE}}
#'
#' @importFrom stats model.frame model.response terms
#' @importFrom survival Surv
#' @export

tteICE <- function(formula, data, strategy='composite', method='np', cov.formula=NULL,
                     weights=NULL,subset=NULL,na.rm=FALSE,nboot=0,seed=0){

  # extract A, Time, cstatus
  if (missing(formula)) stop("`formula` is required, e.g. `formula=Surv.ice(time.p, status.p) ~ A`, or `formula=Surv.ice(time.p, status.p, time.i, status.i) ~ A`")
  if (missing(data)) stop("`data` is required.")

  mf <- model.frame(formula, data = data)
  Y  <- model.response(mf)
  if (!inherits(Y, "Surv.ice")) stop("Use `Surv.ice(time.p, status.p)` or `Surv.ice(time.p, status.p, time.i, status.i)` as dependent variable")

  if (ncol(Y)==2) {
    Time   <- Y[, 1]
    cstatus <- Y[, 2]
  } else {
    Time   <- Y[, 1]
    cstatus <- Y[, 2]
    Time_int   <- Y[, 3]
    status_int <- Y[, 4]
  }

  # if(!all(cstatus %in% c(0,1,2))) stop("The value of status should be set as `0/1` or `0/1/2`")

  # extract treatment variable
  tt <- terms(mf, data = data)
  rhs_vars <- attr(tt, "term.labels")  # e.g. c("A", "X1", "X2")
  if (length(rhs_vars) < 1) stop("Formula must include one treatment variable.")
  if (length(rhs_vars) > 1) stop("Set covariates in `cov.formula=`. Only the first variable is used as treatment variable.")
  A_name <- rhs_vars[1]        # first term is A
  A <- mf[[A_name]]
  # cov1 <- cov1


  # strategy <- match.arg(strategy, c('treatment','composite','natural','removed','whileon','principal'))
  # method <- match.arg(method, c('np','ipw','eff'))
    if(!is.null(cov.formula)){
    if (!inherits(cov.formula, "formula") || length(cov.formula) != 2) {
    stop("`cov.formula` must be a one-sided formula like ~ X1 + X2 + X3")
  }
    mf.cov <- model.frame(cov.formula, data = data)
    tt <- terms(mf.cov, data = data)
    rhs <- attr(tt, "term.labels")
    cov1 <-data[,rhs, drop = FALSE]
    cov1 <- as.matrix(cov1)[subset,]
  } else cov1 <- NULL


  if (!strategy %in% c('treatment','composite','natural','removed','whileon','principal')){
    warning("Please choose a strategy from the following:\n treatment, composite, natural, removed, whileon, principal\n
            composite variable strategy is used by default", call. = FALSE)
    strategy = 'composite'
  }
  if (!method %in% c('np','ipw','eff')){
    warning("Please choose a method from the following:\n np, ipw, eff\n
            nonparametric estimation is used by default", call. = FALSE)
    method = 'np'
  }

  N = length(A)
  if (is.null(weights)) weights = rep(1,N)
  if (is.null(subset)) subset = rep(TRUE,N)
  if (inherits(subset,"logical")) subset = (1:N)[subset]
  if (na.rm){
    cc = complete.cases(data.frame(A,Time,cstatus,weights,subset,cov1))
    cc = (1:N)[cc]
    subset = subset[subset%in%cc]
  }
  if (length(unique(A))!=2) {
    stop('Treatment should be binary!', call. = FALSE)
  } else {
    A = as.numeric(A)
    if (min(A)!=0 | max(A)!=1) {
      A = as.numeric(A==max(A))
      warning(paste0('Treatment should be either 0 or 1! A=1 if A=',max(A)), call. = FALSE)
    }
  }

  A = A[subset]
  Time = Time[subset]
  cstatus = cstatus[subset]
  weights = weights[subset]
  if (ncol(Y)==4){
    Time_int = Time_int[subset]
    status_int = status_int[subset]
  }
  # if (!is.null(cov1)) cov1 = as.matrix(cov1)[subset,]

  # extract covariates
  if(!is.null(cov.formula)){
    if (!inherits(cov.formula, "formula") || length(cov.formula) != 2) {
    stop("`cov.formula` must be a one-sided formula like ~ X1 + X2 + X3")
  }
    mf.cov <- model.frame(cov.formula, data = data)
    tt <- terms(mf.cov, data = data)
    rhs <- attr(tt, "term.labels")
    cov1 = data[,rhs, drop = FALSE]
    cov1 = as.matrix(cov1)[subset,]
  }


  ## estimation
  if(ncol(Y)==2){
    if (method=='ipw') {
        weights = weights*.ipscore(A,cov1,TRUE,weights)
      }
      if (method=='np' | method=='ipw') {
        if (strategy=='treatment') fit = surv.treatment(A,Time,cstatus,weights)
        if (strategy=='composite') fit = surv.composite(A,Time,cstatus,weights)
        if (strategy=='natural') fit = surv.natural(A,Time,cstatus,weights)
        if (strategy=='removed') fit = surv.removed(A,Time,cstatus,weights)
        if (strategy=='whileon') fit = surv.whileon(A,Time,cstatus,weights)
        if (strategy=='principal') fit = surv.principal(A,Time,cstatus,weights)
      } else if (method=='eff') {
        if (strategy=='treatment') fit = surv.treatment.eff(A,Time,cstatus,cov1)
        if (strategy=='composite') fit = surv.composite.eff(A,Time,cstatus,cov1)
        if (strategy=='natural') fit = surv.natural.eff(A,Time,cstatus,cov1)
        if (strategy=='removed') fit = surv.removed.eff(A,Time,cstatus,cov1)
        if (strategy=='whileon') fit = surv.whileon.eff(A,Time,cstatus,cov1)
        if (strategy=='principal') fit = surv.principal.eff(A,Time,cstatus,cov1)
      }

      fit = c(fit,list(A=A,Time=Time,cstatus
        =cstatus,strategy=strategy,cov1=cov1,
                        method=method,weights=weights,na.rm=na.rm,dtype='cmprsk'))
  } else{
    if (method=='np' | method=='ipw') {
    if (strategy=='treatment') fit = scr.treatment(A,Time,cstatus,Time_int,status_int,weights)
    if (strategy=='composite') fit = scr.composite(A,Time,cstatus,Time_int,status_int,weights)
    if (strategy=='natural') fit = scr.natural(A,Time,cstatus,Time_int,status_int,weights)
    if (strategy=='removed') fit = scr.removed(A,Time,cstatus,Time_int,status_int,weights)
    if (strategy=='whileon') fit = scr.whileon(A,Time,cstatus,Time_int,status_int,weights)
    if (strategy=='principal') fit = scr.principal(A,Time,cstatus,Time_int,status_int,weights)
  } else if (method=='eff') {
    if (strategy=='treatment') fit = scr.treatment.eff(A,Time,cstatus,Time_int,status_int,cov1)
    if (strategy=='composite') fit = scr.composite.eff(A,Time,cstatus,Time_int,status_int,cov1)
    if (strategy=='natural') fit = scr.natural.eff(A,Time,cstatus,Time_int,status_int,cov1)
    if (strategy=='removed') fit = scr.removed.eff(A,Time,cstatus,Time_int,status_int,cov1)
    if (strategy=='whileon') fit = scr.whileon.eff(A,Time,cstatus,Time_int,status_int,cov1)
    if (strategy=='principal') fit = scr.principal.eff(A,Time,cstatus,Time_int,status_int,cov1)
  }
  fit = c(fit, list(A=A,Time=Time,status=cstatus,Time_int=Time_int,status_int=status_int,
                    strategy=strategy,cov1=cov1,method=method,
                    weights=weights,na.rm=FALSE,dtype='smcmprsk'))

  }

  if (nboot>-1) fit = surv.boot(fit,nboot,seed)
  n = length(A); n1 = sum(A==1); n0 = sum(A==0)
  fit = c(fit, list(n=n, n1=n1, n0=n0, call= match.call()))
  class(fit) = "tteICE"

  return(fit)
}
