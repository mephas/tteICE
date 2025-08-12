

# library(survival)
## Hidden functions

.matchy <- function(yvec,xvec,newx,exact=FALSE){
  options(warn = -1)
  if (exact) {
    ivec = sapply(newx, function(x) max(which(xvec==x)))
  } else {
    ivec = sapply(newx, function(x) max(which(xvec<=x)))
  }
  if (is.vector(yvec)) {
    newy = yvec[ivec]
  } else {
    newy = yvec[ivec,]
  }
  newy[is.infinite(ivec)] = 0
  return(newy)
}

.ipscore <- function(A,covA){
  fps = glm(A~covA,family='binomial')
  ps = predict(fps,type='response')
  ips = A/ps + (1-A)/(1-ps)
  return(ips)
}

.generatedata <- function(N=500, a1=0.05, a0=0.03, c1=0.04, c0=0.05){
  X1 = rbinom(N,1,0.5)
  X2 = rbinom(N,1,0.5)
  X = cbind(X1,X2)
  A = rbinom(N,1,1/(1+exp(0.5-0.3*X1-0.6*X2)))
  a1x = a1 + X1*0.02 - X2*0.03
  a0x = a0 + X1*0.02 - X2*0.01
  c1x = c1 + X1*0.01 - X2*0.02
  c0x = c0 - X1*0.01 - X2*0.01
  T1 = rweibull(N, 2, sqrt(2/a1x))
  T0 = rweibull(N, 2, sqrt(2/a0x))
  R1 = rexp(N, c1x)
  R0 = rexp(N, c0x)
  C = runif(N,4,8)
  C[C>7] = 7
  T = T1*A + T0*(1-A)
  R = R1*A + R0*(1-A)
  R[R>=T] = 99
  dT = as.numeric(T <= C)
  dR = as.numeric(R <= C)
  T = T*dT + C*(1-dT)
  R = R*dR + C*(1-dR)
  Time = (T+R-abs(T-R))/2
  cstatus = dT + 2*dR
  cstatus[cstatus>2] = 2
  return(list(Z=A,T=T,R=R,dT=dT,dR=dR,Time=Time,cstatus=cstatus,X=X))
}



.phfit_d = function(Tr,Dr,Td,Dd,A,X,a,par=NULL){
  X0 = X
  Tr = Tr[A==a]
  Td = Td[A==a]
  Dr = Dr[A==a]
  Dd = Dd[A==a]
  tt = sort(unique(c(Td[Dd==1],Tr[Dr==1])))
  l = length(tt)
  k = length(Td)
  if (!is.null(X)){
    X = as.matrix(as.matrix(X)[A==a,])
    p = ncol(X)
    upb = TRUE
  } else {
    X = as.matrix(rep(0,k))
    p = 0
    upb = FALSE
  }
  if (!is.null(par)){
    beta = beta0 = par[1+1:p]
    delta_r = delta_r0 = par[1]
  } else {
    beta = beta0 = rep(0, max(p,1))
    delta_r = delta_r0 = 0
  }
  Xb = as.numeric(X%*%beta)
  lam = sapply(tt, function(t) sum(Dd*(Td==t))/sum((Td>=t)*
                                                     exp(Xb+(Tr<t)*delta_r)))
  lam[is.nan(lam)] = 0
  while(TRUE){
    lam0 = lam
    Lam = sapply(1:k, function(i) sum((tt<=Td[i])*lam*
                                        exp(Xb[i]+(tt>Tr[i])*delta_r)))
    dddelta_r = - sum(sapply(1:k, function(i) sum((tt<=Td[i])*lam*
                                                    (tt>Tr[i])*exp(Xb[i]+delta_r))))
    ddelta_r = dddelta_r + sum(Dd*(Tr<Td))
    if (sum(Dd*(Tr<Td))==0) {
      delta_r = 0
    } else {
      delta_r = delta_r - ddelta_r/dddelta_r
    }
    if (upb) {
      dbeta = t(X)%*%(Dd-Lam)
      ddbeta = -t(X)%*%diag(Lam)%*%X
      beta = beta - ginv(ddbeta) %*% dbeta
      Xb = as.numeric(X%*%beta)
    }
    lam = sapply(tt, function(t) sum(Dd*(Td==t))/sum((Td>=t)*
                                                       exp(Xb+(Tr<t)*delta_r)))
    lam[is.nan(lam)] = 0
    tol = max(abs(c(beta-beta0,delta_r-delta_r0,lam-lam0)))
    #print(c(delta_r,beta,tol))
    if (tol<0.00001) break
    beta0 = beta; delta_r0 = delta_r
  }
  if (upb) {
    Xb = X0%*%beta
  } else {
    Xb = rep(0,length(A))
  }
  return(list(beta=beta,delta_r=delta_r,tt=tt,lam=lam,Xb=Xb))
}

.phfit_r = function(Tr,Dr,Td,Dd,A,X,a,par=NULL){
  X0 = X
  Tr = Tr[A==a]
  Td = Td[A==a]
  Dr = Dr[A==a]
  Dd = Dd[A==a]
  tt = sort(unique(Tr[Dr==1]))
  l = length(tt)
  k = length(Tr)
  if (!is.null(X)){
    X = as.matrix(as.matrix(X)[A==a,])
    p = ncol(X)
    upb = TRUE
  } else {
    X = as.matrix(rep(0,k))
    p = 0
    upb = FALSE
  }
  if (!is.null(par)){
    beta = beta0 = par[1:p]
  } else {
    beta = beta0 = rep(0, max(p,1))
  }
  Xb = as.numeric(X%*%beta)
  lam = sapply(tt, function(t) sum(Dr*(Tr==t))/sum((Tr>=t)*exp(Xb)))
  lam[is.nan(lam)] = 0
  while(TRUE){
    lam0 = lam
    Lam = sapply(1:k, function(i) sum((tt<=Tr[i])*lam))
    Lam = Lam * exp(Xb)
    if (upb){
    dbeta = t(X)%*%(Dr-Lam)
    ddbeta = -t(X)%*%diag(Lam)%*%X
    beta = beta - ginv(ddbeta) %*% dbeta
    Xb = as.numeric(X%*%beta)
    }
    lam = sapply(tt, function(t) sum(Dr*(Tr==t))/sum((Tr>=t)*exp(Xb)))
    lam[is.nan(lam)] = 0
    tol = max(abs(c(beta-beta0,lam-lam0)))
    #print(c(beta,tol))
    if (tol<0.00001) break
    beta0 = beta
  }
  if (upb) {
    Xb = X0%*%beta
  } else {
    Xb = rep(0,length(A))
  }
  return(list(beta=beta,tt=tt,lam=lam,Xb=Xb))
}

.phfit_c = function(Tr,Dr,Td,Dd,A,X,a,par=NULL){
  X0 = X
  Td = Td[A==a]
  Dd = Dd[A==a]
  Tc = Td
  Dc = 1 - Dd
  tt = sort(unique(Td[Dd==0]))
  l = length(tt)
  k = length(Tc)
  if (!is.null(X)){
    X = as.matrix(as.matrix(X)[A==a,])
    p = ncol(X)
    upb = TRUE
  } else {
    X = as.matrix(rep(0,k))
    p = 0
    upb = FALSE
  }
  if (!is.null(par)){
    beta = beta0 = par
  } else {
    beta = beta0 = rep(0, max(p,1))
  }
  if (l==0) {
    return(list(beta=beta,tt=0,lam=0))
  }
  Xb = as.numeric(X%*%beta)
  lam = sapply(tt, function(t) sum(Dc*(Tc==t))/sum((Tc>=t)*exp(Xb)))
  lam[is.nan(lam)] = 0
  while(TRUE){
    lam0 = lam
    Lam = sapply(1:k, function(i) sum((tt<=Tc[i])*lam)) * exp(Xb)
    if (upb) {
      dbeta = t(X)%*%(Dc-Lam)
      ddbeta = -t(X)%*%diag(Lam)%*%X
      beta = beta - ginv(ddbeta) %*% dbeta
      Xb = as.numeric(X%*%beta)
    }
    lam = sapply(tt, function(t) sum(Dc*(Tc==t))/sum((Tc>=t)*exp(Xb)))
    lam[is.nan(lam)] = 0
    tol = max(abs(c(beta-beta0,lam-lam0)))
    #print(c(beta,tol))
    if (tol<0.00001) break
    beta0 = beta
  }
  if (upb) {
    Xb = X0%*%beta
  } else {
    Xb = rep(0,length(A))
  }
  return(list(beta=beta,tt=tt,lam=lam,Xb=Xb))
}
