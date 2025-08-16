

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

.phfit <- function(Tr,Dr,Td,Dd,A,X,a){
  Tr = Tr[A==a]
  Td = Td[A==a]
  Dr = Dr[A==a]
  Dd = Dd[A==a]
  Dr[Tr==max(Tr)] = 0
  Dd[Td==max(Td)] = 0
  tt = sort(unique(c(Td,Tr[Dr==1])))
  maxt = max(tt)*0.9
  L = length(tt)
  N = length(Td)
  if (!is.null(X)) {
    X = as.matrix(as.matrix(X)[A==a,])
    p = ncol(X)
    betad = rep(0, p)
    betar = rep(0, p)
  } else {
    p = 0
    betad = betar = 0
  }
  lamd = rep(1/L, L)
  lamr = rep(1/L, L)
  
  # d
  eXb = rep(1, N)
  delta_r = 0
  iter = 0; tol = 1
  while(iter<100){
    iter = iter+1
    est0 = c(betad,delta_r,lamd[tt<maxt])
    S0 = sapply(Td, function(l) sum((Td>=l)*eXb*exp(delta_r*Dr*(Tr<l))))
    if (!is.null(X)){
      S1 = t(sapply(Td, function(l) colSums((Td>=l)*eXb*exp(delta_r*Dr*(Tr<l))*X)))
      S2 = t(sapply(Td, function(l) t(X)%*%diag((Td>=l)*eXb*exp(delta_r*Dr*(Tr<l)))%*%X))
      if (p==1) {S1=t(S1);S2=t(S2)}
      dbeta = as.numeric(t(X-S1/S0)%*%Dd)
      ddbeta = t(S1/S0)%*%diag(Dd)%*%(S1/S0) - matrix(colSums(Dd*S2/S0),p,p)
      betad = betad - ginv(ddbeta) %*% dbeta
      eXb = exp(as.numeric(X%*%betad))
    }
    if (sum(Dd*(Tr<Td))>0) {
      S1 = sapply(Td, function(l) sum((Td>=l)*eXb*exp(delta_r*Dr*(Tr<l))*Dr*(Tr<l)))
      ddelta_r = sum(Dd*((Td>Tr)*Dr-S1/S0))
      dddelta_r = sum(Dd*(S1/S0)^2) - sum(Dd*S1/S0)
      delta_r = delta_r - ddelta_r/dddelta_r
    }
    delta_r = sign(delta_r)*min(abs(delta_r),5)
    lamd = sapply(tt, function(t) sum(Dd*(Td==t))/sum((Td>=t)*eXb*exp((Tr<t)*delta_r)))
    lamd[is.nan(lamd)] = 0
    est = c(betad,delta_r,lamd[tt<maxt])
    tol = max(abs(est-est0))
    if (tol<0.00001) break
  }
  delta_rd = delta_r
  
  # r
  eXb = rep(1, N)
  iter = 0; tol = 1
  while(iter<100){
    iter = iter+1
    est0 = c(betar,lamr[tt<maxt])
    S0 = sapply(Tr, function(l) sum((Tr>=l)*eXb))
    if (!is.null(X)){
      S1 = t(sapply(Tr, function(l) colSums((Tr>=l)*eXb*X)))
      S2 = t(sapply(Tr, function(l) t(X)%*%diag((Tr>=l)*eXb)%*%X))
      if (p==1) {S1=t(S1);S2=t(S2)}
      dbeta = as.numeric(t(X-S1/S0)%*%Dr)
      ddbeta = t(S1/S0)%*%diag(Dr)%*%(S1/S0) - matrix(colSums(Dr*S2/S0),p,p)
      betar = betar - ginv(ddbeta) %*% dbeta
      eXb = exp(as.numeric(X%*%betar))
    }
    lamr = sapply(tt, function(t) sum(Dr*(Tr==t))/sum((Tr>=t)*eXb))
    lamr[is.nan(lamr)] = 0
    est = c(betar,lamr[tt<maxt])
    tol = max(abs(est-est0))
    if (tol<0.00001) break
  }
  
  if (!is.null(X)){
    betad = as.numeric(betad)
    betar = as.numeric(betar)
    Xbd = as.numeric(X%*%betad)
    Xbr = as.numeric(X%*%betar)
  } else {
    betad = betar = 0
    Xbd = Xbr = rep(0,N)
  }
  return(list(betad=betad, betar=betar, delta=delta_rd, 
              Xbd=Xbd, Xbr=Xbr, tt=tt, lamd=lamd, lamr=lamr))
}

.phfit_c <- function(Tr,Dr,Td,Dd,A,X,a){
  Td = Td[A==a]
  Dd = Dd[A==a]
  Dd[Td==max(Td)] = 1
  Dc = 1 - Dc
  tt = sort(unique(Td[Dc==1]))
  maxt = max(tt)*0.9
  L = length(tt)
  N = length(Td)
  if (!is.null(X)) {
    X = as.matrix(as.matrix(X)[A==a,])
    p = ncol(X)
    beta = rep(0, p)
  } else {
    p = 0
    beta = 0
  }
  lam = rep(1/L, L)
  eXb = rep(1, N)
  iter = 0; tol = 1
  while(iter<100){
    iter = iter+1
    est0 = c(beta,lam[tt<maxt])
    S0 = sapply(Td, function(l) sum((Td>=l)*eXb))
    if (!is.null(X)){
      S1 = t(sapply(Td, function(l) colSums((Td>=l)*eXb*X)))
      S2 = t(sapply(Td, function(l) t(X)%*%diag((Td>=l)*eXb)%*%X))
      if (p==1) {S1=t(S1);S2=t(S2)}
      dbeta = as.numeric(t(X-S1/S0)%*%Dr)
      ddbeta = t(S1/S0)%*%diag(Dr)%*%(S1/S0) - matrix(colSums(Dr*S2/S0),p,p)
      beta = beta - ginv(ddbeta) %*% dbeta
      eXb = exp(as.numeric(X%*%beta))
    }
    lam = sapply(tt, function(t) sum(Dc*(Td==t))/sum((Td>=t)*eXb))
    lam[is.nan(lam)] = 0
    est = c(beta,lam[tt<maxt])
    tol = max(abs(est-est0))
    if (tol<0.00001) break
  }
  
  if (!is.null(X)){
    beta = as.numeric(beta)
    Xb = as.numeric(X%*%beta)
  } else {
    beta = 0
    Xb = rep(0,N)
  }
  return(list(beta=beta, Xb=Xb, tt=tt, lam=lam))
}
