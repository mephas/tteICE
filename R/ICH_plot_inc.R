
#' @title plot
#'
#' @description Plot 
#'
#' @param fit fit
#'
#' @param decrease decrease
#'
#' @param conf.int conf
#'
#' @param nboot nboot
#'
#' @param seed seed
#'
#' @param xlab xlab
#'
#' @param xlim xlim
#'
#' @param ylim ylim
#' 
#' @param legend legend
#' 
#' @param cex cex
#' 
#'
#' @param ... Other augments in function \code{\link{plot.default}} or function \code{\link{curve}}
#'
#' @importFrom graphics plot abline points
#' @importFrom stats qnorm
#'
#' @return curves
#'
#' @seealso
#' \code{\link[graphics]{plot.default}},
#' \code{\link[graphics]{points}},
#' \code{\link[graphics]{curve}},
#'
#'
#' @examples
#' plot(1,1)
#'
#' @export

plot.inc <- function(fit,decrease=FALSE,conf.int=.95,nboot=0,seed=0,xlab='Time',xlim=NULL,
                     ylim=c(0,1),legend=c('Treated','Control'),cex=0.9,...){
  if (fit$strategy=='treatment') stname = 'Treatment policy'
  if (fit$strategy=='composite') stname = 'Composite variable'
  if (fit$strategy=='natural') stname = 'Hypothetical I (natural)'
  if (fit$strategy=='removed') stname = 'Hypothetical II (removed)'
  if (fit$strategy=='whileon') stname = 'While on treatment'
  if (fit$strategy=='principal') stname = 'Principal stratum'
  if (decrease==TRUE){
    cif1 = 1-fit$cif1
    cif0 = 1-fit$cif0
    x = 'bottomleft'
    ylab = 'Survival probability'
  } else {
    cif1 = fit$cif1
    cif0 = fit$cif0
    x = 'topleft'
    ylab = 'Cumulative incidence'
  }
  if (!is.null(xlim)){
    i1 = fit$time1<=xlim[2]
    i0 = fit$time0<=xlim[2]
    t1 = fit$time1[i1]
    t0 = fit$time0[i0]
    cif1 = cif1[i1]
    cif0 = cif0[i0]
  } else {
    t1 = fit$time1
    t0 = fit$time0
    i1 = rep(TRUE,length(t1))
    i0 = rep(TRUE,length(t0))
  }
  plot(t1,cif1,type='s',col='brown',lwd=2,main=stname,
       xlab=xlab,ylab=ylab,ylim=ylim,...)
  points(t0,cif0,type='s',col='darkcyan',lwd=2)
  if (!is.null(conf.int)){
    fit.b = surv.boot(fit,nboot,seed)
    z = -qnorm((1-conf.int)/2)
    points(t1,cif1+fit.b$se1[i1]*z,type='s',lty=2,lwd=1.5,col='brown')
    points(t1,cif1-fit.b$se1[i1]*z,type='s',lty=2,lwd=1.5,col='brown')
    points(t0,cif0+fit.b$se0[i0]*z,type='s',lty=2,lwd=1.5,col='darkcyan')
    points(t0,cif0-fit.b$se0[i0]*z,type='s',lty=2,lwd=1.5,col='darkcyan')
  }
  legend(x,cex=cex,col=c('brown','darkcyan'),lwd=c(2,2),legend=legend)
}

