#' @title Plotting the estimated function
#'
#' @description This function plots the estimated potential cumulative incidence function
#' with pointwise confidence intervals.
#'
#' @param x A fitted object from \code{surv.ICH} or \code{scr.ICH}.
#' 
#' @param type Which plot to create: \code{ate} indicates to plot the estimated treatment effect; \code{inc} indicates to plot the estimated cumulative incidence function.
#'
#' @param decrease A logical variable indicating whether displaying the cumulative incidence
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
#' @param legend Change the legend of the estimated cumulative incidence function plot. Only valid when \code{type=inc}. 
#' 
#' @param cex Size of legend. 
#'
#' @param ... Other augments in function \code{\link{plot.default}} or function \code{\link{curve}}
#'
#' @examples
#' ## load data
#' data(bmt)
#' bmt = transform(bmt, d4=d2+d3)
#' A = as.numeric(bmt$group>1)
#' ## plot treatment effects with p-values
#' for (st in c('composite','natural','removed','whileon','principal')){
#' fit = surv.ICH(A, bmt$t2, bmt$d4, st)
#' plot(fit, type="inc", ylim=c(0,1))
#' p = fit$p.val
#' if (!is.null(p)) text(200, 0.8, paste0('P = ', round(p,3)))
#' }
#' ## plot counterfactual cumulative incidence functions
#' for (st in c('composite','natural','removed','whileon','principal')){
#' fit = surv.ICH(A, bmt$t2, bmt$d4, st)
#' plot(fit, type="ate", ylim=c(0,1))
#' }
#'
#' @seealso
#' \code{\link{plot_ate}},
#' \code{\link{plot_inc}}
#'
#' 
#' @method plot ICH
#' @export

plot.ICH <- function(x, type=c("ate","inc")[1],decrease=FALSE,conf.int=.95,nboot=0,seed=0,xlab='Time',xlim=NULL,
                     ylim=NULL,legend=c('Treated','Control'),cex=0.9,...){


  if(type=="ate") {
    if(is.null(ylim)) ylim=c(-1,1)
    plot_ate(fit=x,decrease=decrease,conf.int=conf.int,nboot=nboot,seed=seed,xlab=xlab,xlim=xlim,ylim=ylim,...)
    }

  if(type=="inc") {
    if(is.null(ylim)) ylim=c(-1,1)
    plot_inc(fit=x,decrease=decrease,conf.int=conf.int,nboot=nboot,seed=seed,xlab=xlab, xlim=xlim,ylim=ylim,legend=legend,cex=cex,...)
    }


}
