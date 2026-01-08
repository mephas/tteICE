#' tteICE: Treatment Effect Estimation for Time-to-Event Data with Intercurrent Events
#'
#' Main functions:
#' \itemize{
#' 	 \item \code{\link[tteICE]{tteICE}} Fit the CIF for time-to-event data using formula
#'   \item \code{\link[tteICE]{scr.tteICE}} Fit the CIF for time-to-event data with intercurrent events for semicompeting risks data
#'   \item \code{\link[tteICE]{surv.tteICE}} Fit the CIF for time-to-event with intercurrent events for competing risks data
#' 	 \item \code{\link[tteICE]{plot.tteICE}} Graphical results of tteICE
#'   \item \code{\link[tteICE]{print.tteICE}} Print a short summary of the estimated treatment effect
#'   \item \code{\link[tteICE]{summary.tteICE}} Summary of tteICE
#'   \item \code{\link[tteICE]{riskpredict}} Risk prediction at specific time points
#'   \item \code{\link[tteICE]{tteICEShiny}} Interactive Shiny app for tteICE
#' }
#' Example data:
#' \itemize{
#'   \item \code{\link{bmt}} Data from Section 1.3 of Klein and Moeschberger (1997)
#' }
#'
#' @docType package
#' @name tteICE-package
#' 
#' @importFrom lifecycle deprecated
"_PACKAGE"
