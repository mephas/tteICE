#' @title Estimate hazard ratios
#'
#' @description This function estimates the hazard ratio
#' for time-to event data under ICH E9 (R1) to address intercurrent events. Multiple
#' strategies except the principal stratum strategy are allowed.
#'
#' @param A Treatment indicator, 1 for treatment and 0 for control.
#'
#' @param Time Time to event.
#'
#' @param cstatus Indicator of event, 1 for the primary event, 2 for the intercurrent event, 0 for censoring.
#'
#' @param strategy Strategy to address intercurrent events, \code{"treatment"} indicating treatment policy strategy,
#' \code{"composite"} indicating composite variable strategy, \code{"natural"} indicating hypothetical strategy
#' (Scenario I, controlling the hazard of intercurrent events), \code{"removed"} indicating hypothetical strategy
#' (Scenario II, removing intercurrent events), and \code{"whileon"} indicating while on treatment strategy.
#'
#' @param cov1 Baseline covariates.
#'
#' @param conf.int Level of the confidence interval.
#'
#' @param weights Weight for each subject (not applied to the while on treatment strategy).
#'
#' @param subset Subset, either numerical or logical.
#'
#'
#' @return A list including
#' \describe{
#' \item{logHR}{Estimated log hazard ratio (logHR) of the treatment effect on the primary event.}
#' \item{se}{Standard error of the estimated log hazard ratio (logHR).}
#' \item{CI}{Confidence interval of the hazard ratio (HR).}
#' \item{p.val}{P value of the hazard ratio.}
#' }
#'
#' @examples
#' data(bmt)
#' bmt = transform(bmt, d4=d2+d3)
#' A = as.numeric(bmt$group>1)
#' 
#' ## composite variable strategy
#' fit = surv.HR(A, bmt$t2, bmt$d4, "composite")
#' 
#' ## while on treatment strategy
#' X = bmt[,c('z1','z3','z5')]
#' fit = surv.HR(A, bmt$t2, bmt$d4, "whileon", cov1=X)
#' 
#'
#' @details
#' \describe{
#' For the treatment policy and hypothetical strategies, the hazard ratio (HR) is given by the Cox
#' regression regarding intercurrent events as censoring. For the composite variable strategy, the
#' hazard ratio is given by the Cox regression regarding the first occurrence of either intercurrent
#' event or primary event as the event of interest. For the while on treatment strategy, the hazard
#' ratio is given by the Fine-Gray subdistribution model. There is no existing method to estimate the
#' hazard ratio using principal stratum strategy. \cr
#' The weakness of using hazard ratio to infer treatment effects is critical. First, the hazard ratio
#' relies on model specification. Second, the hazard ratio is not collapsible. Therefore, the hazard
#' ratio should only be treated as a descriptive or exploratory measure of the treatment effect.
#' }
#'
#'
#' @export

surv.HR <- function(A,Time,cstatus,strategy='composite',cov1=NULL,
                    conf.int=0.95,weights=NULL,subset=NULL){
  if (strategy %in% c('treatment','natural','removed')){
    fit = coxph(Surv(Time,cstatus==1)~cbind(A,cov1), weights=weights, subset=subset)
  }
  if (strategy=='composite'){
    fit = coxph(Surv(Time,cstatus>0)~cbind(A,cov1), weights=weights, subset=subset)
  }
  if (strategy=='whileon'){
    if (!is.null(subset)) {
    fit = crr(Time,cstatus,cov1=cbind(A,cov1),subset=subset)
    } else {
      fit = crr(Time,cstatus,cov1=cbind(A,cov1))
    }
    fit$coefficients = fit$coef
  }
  if (strategy=='principal') {
    return(list(HR=NULL,se=NULL,CI=NULL,p.val=NULL,strategy=strategy))
  }
  HR = fit$coefficients[1]
  se = sqrt(fit$var[1,1])
  z = -qnorm((1-conf.int)/2)
  CI = exp(c(HR-z*se,HR+z*se))
  names(HR) = names(se) = NULL
  names(CI) = c('Lower','Upper')
  p = 2*pnorm(-abs(HR/se))

  return(list(logHR=HR,se=se,CI=CI,p.val=p,strategy=strategy))
}
