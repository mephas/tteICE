#' @title Plotting the estimated treatment effect curve
#'
#' @description This function plots the estimated treatment effect (difference in potential
#' cumulative incidences under treated and control) with pointwise confidence intervals.
#'
#' @param fit A fitted object from \code{surv.ICH}.
#'
#' @param decrease A logical variable indicating whether to display the difference in cumulative
#' incidence functions (\code{decrease = FALSE}) or survival functions (\code{decrease = TRUE}).
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
#' @param ... Other augments in function \code{\link{plot.default}} or function \code{\link{curve}}
#'
#' @importFrom graphics plot abline points
#' @importFrom stats qnorm
#'
#' @seealso
#' \code{\link[graphics]{plot.default}},
#' \code{\link[graphics]{points}},
#' \code{\link[graphics]{curve}},
#' \code{\link[tteICE]{plot.ICH}}
#'
#' @examples
#' ## load data and fit the model
#' data(bmt)
#' bmt = transform(bmt, d4=d2+d3)
#' A = as.numeric(bmt$group>1)
#' fit = surv.ICH(A, bmt$t2, bmt$d4, 'composite')
#' ## plot asymptotic confidence intervals based on explicit formulas
#' plot_ate(fit, legend=c('AML','ALL'), ylim=c(-0.4,0.4))
#' ## plot bootstrap confidence intervals
#' plot_ate(fit, nboot=200, legend=c('AML','ALL'), ylim=c(-0.4,0.4))
#'
#' @export

plot_ate <- function(fit,decrease=FALSE,conf.int=.95,nboot=0,seed=0,
  xlab='Time',xlim=NULL,ylim=c(-1,1),...){
  if (fit$strategy=='treatment') stname = 'Treatment policy'
  if (fit$strategy=='composite') stname = 'Composite variable'
  if (fit$strategy=='natural') stname = 'Hypothetical I (natural)'
  if (fit$strategy=='removed') stname = 'Hypothetical II (removed)'
  if (fit$strategy=='whileon') stname = 'While on treatment'
  if (fit$strategy=='principal') stname = 'Principal stratum'
  fit.b = surv.boot(fit,nboot=nboot,seed=seed)
  tt = fit.b$time
  dcif = fit.b$ate
  se = fit.b$se
  ciu = dcif - qnorm((1-conf.int)/2)*se
  cil = dcif + qnorm((1-conf.int)/2)*se
  ylab = 'Difference in CIFs'
  if (decrease==TRUE){
    dcif = -dcif
    ciu = -ciu
    cil = -cil
    ylab = 'Difference in Survivals'
  }
  if (!is.null(xlim)) {
    ti = (tt>=xlim[1])&(tt<=xlim[2])
    tt = tt[ti]
    dcif = dcif[ti]
    ciu = ciu[ti]
    cil = cil[ti]
  }
  plot(tt,dcif,type='s',main=stname,ylim=ylim,xlab=xlab,ylab=ylab,lwd=2,...)
  abline(h=0,lty=2)
  points(tt,ciu,type='s',lty=5,lwd=1.5,col='darkgrey')
  points(tt,cil,type='s',lty=5,lwd=1.5,col='darkgrey')
}
