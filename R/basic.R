

# library(survival)
## Hidden functions

.matchy <- function(yvec,xvec,newx,exact=FALSE){
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
