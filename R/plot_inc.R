#' @title Plotting the estimated cumulative incidence function
#'
#' @description This function plots the estimated potential cumulative incidence function
#' with pointwise confidence intervals.
#'
#' @param fit A fitted object from \code{surv.ICH}.
#'
#' @param decrease A logical variable indicating whether to display the cumulative incidence
#' function (\code{decrease = FALSE}) or survival function (\code{decrease = TRUE}).
#'
#' @param conf.int Level of the confidence interval. If \code{conf.int = NULL}, then the condifence
#' interval will not be provided.
#'
#' @param nboot Number of resamplings in bootstrapping. Default \code{nboot = 0}, using the explicit
#' formula of the standard error.
#'
#' @param seed Seed for bootstrapping.
#' 
#' @param xlab Label for x-axis.
#' 
#' @param xlim Limit for x-axis. 
#' 
#' @param ylim Limit for y-axis. 
#' 
#' @param legend Legend. 
#' 
#' @param cex Size of legend. 
#'
#' @param ... Other augments in function \code{\link{plot.default}} or function \code{\link{curve}}
#'
#' @importFrom graphics plot points
#'
#' @seealso
#' \code{\link[graphics]{plot.default}},
#' \code{\link[graphics]{points}},
#' \code{\link[graphics]{curve}},
#' \code{\link{plot.ICH}}
#'
#' @examples
#' ## load data and fit the model
#' data(bmt)
#' bmt = transform(bmt, d4=d2+d3)
#' A = as.numeric(bmt$group>1)
#' fit = surv.ICH(A, bmt$t2, bmt$d4, 'composite')
#' ## plot asymptotic confidence intervals based on explicit formulas
#' plot_inc(fit, legend=c('AML','ALL'), ylim=c(0,1))
#' ## plot bootstrap confidence intervals
#' plot_inc(fit, nboot=200, legend=c('AML','ALL'), ylim=c(0,1))
#'
#' @export

plot_inc <- function(fit,decrease=FALSE,conf.int=.95,nboot=0,seed=0,xlab='Time',xlim=NULL,
                     ylim=c(0,1),legend=c('Treated','Control'),
                     col=c('brown','darkcyan'),cex=0.9,...){
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
    ti = (fit$Time>=xlim[1])&(fit$Time<=xlim[2])
    tt = fit$Time[ti]
    cif1 = cif1[ti]
    cif0 = cif0[ti]
  } else {
    tt = fit$Time
    ti = rep(TRUE,length(tt))
  }
  col1 = col[1]
  col0 = col[2]
  plot(tt,cif1,type='s',col=col1,lwd=2,main=stname,
       xlab=xlab,ylab=ylab,ylim=ylim,...)
  points(tt,cif0,type='s',col=col0,lwd=2)
  if (!is.null(conf.int)){
    fit.b = surv.boot(fit,nboot,seed)
    z = -qnorm((1-conf.int)/2)
    points(tt,cif1+fit.b$se1[i1]*z,type='s',lty=2,lwd=1.5,col=col1)
    points(tt,cif1-fit.b$se1[i1]*z,type='s',lty=2,lwd=1.5,col=col1)
    points(tt,cif0+fit.b$se0[i0]*z,type='s',lty=2,lwd=1.5,col=col0)
    points(tt,cif0-fit.b$se0[i0]*z,type='s',lty=2,lwd=1.5,col=col0)
  }
  legend(x,cex=cex,col=c(col1,col0),lwd=c(2,2),legend=legend)
}

