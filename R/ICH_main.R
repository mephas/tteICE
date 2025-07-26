

surv.ICH <- function(A,Time,cstatus,strategy='composite',cov1=NULL,method='np',
                     weights=NULL,subset=NULL){
  N = length(A)
  if (is.null(weights)) weights = rep(1,N)
  if (!is.null(cov1)) weights = weights*.ipscore(A,cov1)
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
  return(c(fit,list(A=A,Time=Time,cstatus=cstatus,strategy=strategy,cov1=cov1,
                    method=method,weights=weights,subset=subset)))
}
