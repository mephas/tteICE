#' @title Graphical results of tteICE
#'
#' @description This function plots the estimated potential cumulative incidence functions or treatment effect curve
#' with pointwise confidence intervals.
#'
#' @param x A fitted object returned by the function \code{surv.tteICE} or \code{scr.tteICE}.
#'
#' @param type Which plot to create: \code{ate} indicates to plot the estimated treatment effect; \code{inc} indicates to plot the estimated cumulative incidence function.
#'
#' @param decrease A logical variable indicating the type of curve to display. If \code{decrease = FALSE} (default), the function displays the cumulative incidence functions (CIFs) or their differences. If \code{decrease = TRUE}, the function instead displays the survival functions or their differences.
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
#' @param ylim A numeric vector of length 2 giving the limits of the y-axis. If \code{ylim=NULL} (default), the range is determined automatically by the type of plot
#'
#' @param plot.configs A named \code{list} of additional plot configurations. See details in functions  \code{\link{plot_ate}} and \code{\link{plot_inc}}
#'
#' @param ... Other augments in function \code{\link{plot.default}} or function \code{\link{curve}}
#'
#' @examples
#' ## load data
#' data(bmt)
#' bmt = transform(bmt, d4=d2+d3)
#' A = as.numeric(bmt$group>1)
#' ## plot cumulative incidence functions with p-values
#' for (st in c('composite','natural','removed','whileon','principal')){
#'  fit = surv.tteICE(A, bmt$t2, bmt$d4, st)
#'  plot(fit, type="inc", decrease=TRUE, ylim=c(0,1),
#'       plot.configs=list(show.p.value=TRUE))
#' }
#' ## plot treatment effects for semicompeting risk data
#' for (st in c('composite','natural','removed','whileon','principal')){
#'  fit = scr.tteICE(A, bmt$t1, bmt$d1, bmt$t2, bmt$d2, st)
#'  plot(fit, type="ate", ylim=c(-1,1), xlab="time", 
#'       plot.configs=list(col="red"))
#' }
#'
#' @seealso \code{\link[tteICE]{plot_ate}}, \code{\link[tteICE]{plot_inc}}
#'
#'
#' @method plot tteICE
#' @return Plot the results from a tteICE object
#' @export

plot.tteICE <- function(x, type=c("ate","inc")[1],
  decrease=FALSE,conf.int=.95,nboot=0,seed=0, xlab='Time',xlim=NULL, ylim=NULL,
  plot.configs=list(),...){

  if (!inherits(x, "tteICE"))  stop("`fit` must be an object returned by `surv.tteICE` or `scr.tteICE`.", call. = FALSE)
  type <- match.arg(type, c("ate", "inc"))

  if(type=="ate") {
    if(is.null(ylim)) ylim=c(-1,1) else ylim=ylim
    plot_ate(fit=x,decrease=decrease,conf.int=conf.int,nboot=nboot,seed=seed,xlab=xlab,xlim=xlim,ylim=ylim,
      plot.configs=plot.configs,...)
    }
  else {
    if(is.null(ylim)) ylim=c(0,1) else ylim=ylim
    plot_inc(fit=x,decrease=decrease,conf.int=conf.int,nboot=nboot,seed=seed,xlab=xlab, xlim=xlim,ylim=ylim,
      plot.configs=plot.configs,...)
    }
}
