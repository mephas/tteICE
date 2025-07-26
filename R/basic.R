

# library(survival)
## Hidden functions

.matchy <- function(yvec,xvec,newx){
  ivec = sapply(newx, function(x) max(which(xvec<=x)))
  if (is.vector(yvec)) {
    return(yvec[ivec])
  } else {
    return(yvec[ivec,])
  }
}

.ipscore <- function(A,covA){
  fps = glm(A~covA,family='binomial')
  ps = predict(fps,type='response')
  ips = A/ps + (1-A)/(1-ps)
  return(ips)
}
