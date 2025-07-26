
surv.boot <- function(fit,nboot=0,seed=0){
  N = length(fit$A)
  time1 = fit$time1
  time0 = fit$time0
  Time = sort(unique(c(0,fit$Time[fit$cstatus>0],max(fit$Time))))
  cif1 = fit$cif1
  cif0 = fit$cif0
  se1 = fit$se1
  se0 = fit$se0
  ate = matchy(fit$ate,fit$time,Time)
  se = matchy(fit$se,fit$time,Time)
  if (nboot>1){
    cif1l = cif0l = te = NULL
    set.seed(seed)
    for(b in 1:nboot){
      #wt = as.vector(rmultinom(1,N,rep(1/N,N)))
      subset = sample(1:N,replace=TRUE)
      fitb = surv.ICH(fit$A,fit$Time,fit$cstatus,fit$strategy,fit$cov1,fit$method,
                      fit$weights,subset)
      cifb1 = matchy(fitb$cif1,fitb$time1,Time)
      cifb0 = matchy(fitb$cif0,fitb$time0,Time)
      cif1l = rbind(cif1l, cifb1)
      cif0l = rbind(cif0l, cifb0)
      te = rbind(te, cifb1-cifb0)
    }
    se1 = apply(cif1l,2,sd)
    se0 = apply(cif0l,2,sd)
    se1 = matchy(se1,Time,time1)
    se0 = matchy(se0,Time,time0)
    se = apply(te,2,sd)
  }
  return(list(time1=time1,time0=time0,cif1=fit$cif1,cif0=fit$cif0,
              se1=se1,se0=se0,time=Time,ate=ate,se=se,
              strategy=fit$strategy,method=fit$method))
}

