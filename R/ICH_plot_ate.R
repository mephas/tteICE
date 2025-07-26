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


plot.ate <- function(fit,decrease=FALSE,conf.int=.95,nboot=0,seed=0,xlab='Time',
                     xlim=NULL,ylim=c(-1,1),...){
  if (fit$strategy=='treatment') stname = 'Treatment policy'
  if (fit$strategy=='composite') stname = 'Composite variable'
  if (fit$strategy=='natural') stname = 'Hypothetical I (natural)'
  if (fit$strategy=='removed') stname = 'Hypothetical II (removed)'
  if (fit$strategy=='whileon') stname = 'While on treatment'
  if (fit$strategy=='principal') stname = 'Principal stratum'
  fit.b = surv.boot(fit,nboot=nboot,seed=seed)
  tm = fit.b$time
  dcif = fit.b$ate
  se = fit.b$se
  ciu = dcif - qnorm((1-conf.int)/2)*se
  cil = dcif + qnorm((1-conf.int)/2)*se
  ylab = 'Diff in cumulative incidences'
  if (decrease==TRUE){
    dcif = -dcif
    ciu = -ciu
    cil = -cil
    ylab = 'Diff in survival probabilities'
  }
  if (!is.null(xlim)) {
    id = tm<=xlim[2]
    tm = tm[id]
    dcif = dcif[id]
    ciu = ciu[id]
    cil = cil[id]
  }
  plot(tm,dcif,type='s',main=stname,ylim=ylim,xlab=xlab,ylab=ylab,lwd=2,...)
  abline(h=0,lty=2)
  points(tm,ciu,type='s',lty=5,lwd=1.5,col='brown')
  points(tm,cil,type='s',lty=5,lwd=1.5,col='brown')
}
