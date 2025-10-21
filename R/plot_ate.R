#' @title Plot the estimated treatment effect
#'
#' @description This function plots the estimated treatment effect, defined as the difference in potential cumulative incidences under treated and control groups, along with pointwise confidence intervals.
#'
#' @param fit A fitted object returned by the function \code{surv.tteICE} or \code{scr.tteICE}.
#'
#' @param decrease A logical variable indicating the type of curve difference to display. If \code{decrease = FALSE} (default), the function displays the difference in cumulative incidence functions (CIFs). If \code{decrease = TRUE}, the function instead displays the difference in survival functions.
#'
#' @param conf.int Confidence level for the interval. If \code{conf.int = NULL}, no confidence interval is provided.
#'
#' @param nboot Number of resampling in bootstrapping. By default, \code{nboot = 0}, meaning no bootstrap is performed and the standard error is computed using the explicit analytical formula.
#'
#' @param seed Sets the random seed used when generating bootstrap samples.
#'
#' @param xlab Label for x-axis.
#'
#' @param xlim A numeric vector of length 2 giving the limits of the x-axis. If \code{xlim=NULL} (default), the range is determined automatically from the data.
#'
#' @param ylim A numeric vector of length 2 giving the limits of the y-axis. Defaults to \code{ylim=c(-1, 1)}.
#'
#' @param plot.configs A named \code{list} of additional plot configurations. Common entries include:
#'
#' \itemize{
#'     \item \code{ylab}: character, label for the y-axis (default: \code{ylab=NULL}, use the default label).
#'     \item \code{main}: character, title for the plot (default: \code{main=NULL}, use the default label).
#'     \item \code{lty}: line type for effect curve (default: 1).
#'     \item \code{lwd}: line width for effect curve (default: 2).
#'     \item \code{col}: line color for effect curve (default: "black").
#'     \item \code{add.null.line}: logical, whether to draw a horizontal line at 0 (default: TRUE).
#'     \item \code{null.line.lty}: line type for horizontal line at 0 (default: 2).
#'     \item \code{ci.lty}: line type for confidence interval curves (default: 5).
#'     \item \code{ci.lwd}: line width for confidence interval curves (default: 1.5).
#'     \item \code{ci.col}: line color for confidence interval curves (default: "darkgrey").
#'   }
#'
#' @param ... Additional graphical arguments passed to function \code{\link{plot.default}} or function \code{\link{curve}}
#'
#' @importFrom graphics plot abline points legend
#' @importFrom stats qnorm
#'
#' @seealso
#' \code{\link[graphics]{plot.default}},
#' \code{\link[graphics]{points}},
#' \code{\link[graphics]{curve}},
#' \code{\link[tteICE]{plot.tteICE}}
#'
#' @examples
#' ## Load data and fit the model
#' data(bmt)
#' bmt = transform(bmt, d4=d2+d3)
#' A = as.numeric(bmt$group>1)
#' ## Model with competing risk data
#' fit1 = surv.tteICE(A, bmt$t2, bmt$d4, 'composite')
#' ## Plot asymptotic confidence intervals based on explicit formulas
#' plot_ate(fit1, ylim=c(-0.4,0.4))
#' \donttest{
#' ## Plot bootstrap confidence intervals (may take some seconds)
#' plot_ate(fit1, nboot=200, ylim=c(-0.4,0.4))
#' }
#' ## Model with semicompeting risk data
#' fit2 = scr.tteICE(A, bmt$t1, bmt$d1, bmt$t2, bmt$d2, "composite")
#' ## Plot asymptotic confidence intervals based on explicit formulas
#' plot_ate(fit2, ylim=c(-0.4,0.4),
#'          plot.configs=list(add.null.line=FALSE))
#' ## Plot bootstrap confidence intervals
#' plot_ate(fit2, nboot=200, ylim=c(-0.4,0.4),
#'          plot.configs=list(add.null.line=FALSE, lty=2, main=""))
#'
#' @return Plot the averaget treatment effect (ATE) results from a tteICE object
#' @export

plot_ate <- function(fit,decrease=FALSE,conf.int=.95,nboot=0,seed=0,
  xlab='Time',ylim=c(-1,1),xlim=NULL,
  plot.configs=list(ylab=NULL, main=NULL,
                    lty=1, lwd=2, col="black",
                    add.null.line=TRUE, null.line.lty=2,
                    ci.lty=5, ci.lwd=1.5, ci.col="darkgrey"),...){

  #---- validate input ----#
  adj <- .plot_ate_validate(fit, decrease, conf.int, nboot, seed, xlab, xlim, ylim)
  conf.int <- adj$conf.int

  #---- set configurations ----#
  default.configs <- list(
    ylab=NULL,main=NULL,
    lty=1, lwd=2, col="black",
    add.null.line=TRUE, null.line.lty=2,
    ci.lty=5, ci.lwd=1.5, ci.col="darkgrey")
  plot.configs <- modifyList(default.configs, plot.configs)

  #---- plot ----#
  if(is.null(plot.configs[["main"]])){
  if (fit$strategy=='treatment') stname = 'Treatment policy strategy'
  if (fit$strategy=='composite') stname = 'Composite variable strategy'
  if (fit$strategy=='natural') stname = 'Hypothetical I (natural) strategy'
  if (fit$strategy=='removed') stname = 'Hypothetical II (removed) strategy'
  if (fit$strategy=='whileon') stname = 'While on treatment strategy'
  if (fit$strategy=='principal') stname = 'Principal stratum strategy'
} else stname=plot.configs[["main"]]

  fit.b = surv.boot(fit,nboot=nboot,seed=seed)
  tt = fit.b$time
  dcif = fit.b$ate
  se = fit.b$se
  ciu = dcif - qnorm((1-conf.int)/2)*se
  cil = dcif + qnorm((1-conf.int)/2)*se
  if(is.null(plot.configs[["ylab"]])) ylab = 'Difference in CIFs' else ylab=plot.configs[["ylab"]]

  if (decrease==TRUE){
    dcif = -dcif
    ciu = -ciu
    cil = -cil
    if(is.null(plot.configs[["ylab"]])) ylab = 'Difference in Survivals' else ylab=plot.configs[["ylab"]]
  }
  if (!is.null(xlim)) {
    ti = (tt>=xlim[1])&(tt<=xlim[2])
    tt = tt[ti]
    dcif = dcif[ti]
    ciu = ciu[ti]
    cil = cil[ti]
  }

  #---- main plot ----#
  plot(tt,dcif,type='s',main=stname,ylim=ylim,xlab=xlab,ylab=ylab,lty=plot.configs[["lty"]], lwd=plot.configs[["lwd"]],col=plot.configs[["col"]],...)
  points(tt,ciu,type='s',lty=plot.configs[["ci.lty"]],lwd=plot.configs[["ci.lwd"]],col=plot.configs[["ci.col"]])
  points(tt,cil,type='s',lty=plot.configs[["ci.lty"]],lwd=plot.configs[["ci.lwd"]],col=plot.configs[["ci.col"]])
  if(plot.configs[["add.null.line"]]) abline(h=0,lty=plot.configs[["null.line.lty"]])
}
