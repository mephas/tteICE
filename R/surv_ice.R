#' @title Outcome def
#'
#' @description TOutcome def
#'
#' @param time.p Treatment indicator, 1 for treatment and 0 for control.
#'
#' @param status.p Time to the primary (terminal) event.
#'
#' @param time.i Indicator of the primary (terminal) event, 1 for event and 0 for censoring.
#'
#' @param status.i Time to the intercurrent event.
#'
#' @return A list including the fitted object and input variables.
#'
#' @examples
#' data(bmt)
#' bmt = transform(bmt, d4=d2+d3)
#' with(bmt,Surv.ice(t1,d1))
#' with(bmt,Surv.ice(t1,d4))
#' with(bmt,Surv.ice(t1,d1,t2,d2))
#' @export

Surv.ice <- function(time.p, status.p, time.i = NULL, status.i = NULL) {

  # ---- basic coercion ----
  time.p <- as.numeric(time.p)
  status.p <- as.integer(status.p)

  n <- length(time.p)
  if (length(status.p) != n) stop("time.p and status.p must have the same length.")
  if (anyNA(time.p) || anyNA(status.p)) stop("Missing values not allowed in Surv_ice().")

  # ---- status check for status.p ----
  if (!all(status.p %in% c(0,1,2))) {
    stop("status.p must be in c(0,1,2) or c(0,1).")
  }

  # ---- detect 2-arg vs 4-arg ----

  is_two_arg <- !is.null(time.p) && !is.null(status.p) && is.null(time.i) && is.null(status.i)
  is_four_arg <- !is.null(time.p) && !is.null(status.p) && !is.null(time.i) && !is.null(status.i)

  if (!is_two_arg && !is_four_arg) {
    stop("Use Surv.ice(time.p, status.p) OR Surv.ice(time.p, status.p, time.i, status.i).")
  }

  ## if status.p %in% c(0,1,2) then time.i and status.i are ignored
  if(any(status.p %in% c(2)) && (!is.null(time.i) || !is.null(time.i))){
    time.i = NULL; status.i = NULL
    warning("Only use Surv.ice(time.p, status.p) as outcomes")
  }

  ## if status.p %in% c(0,1) then time.i and status.i are required??

  if (is_two_arg) {
    Y <- cbind(time.p = time.p, status.p = status.p)
    class(Y) <- c("Surv.ice", "matrix")
    attr(Y, "type") <- "cmprsk"
    return(Y)
  }

  # ---- 4-arg case ----
  if(is_four_arg){
  time.i <- as.numeric(time.i)
  status.i <- as.integer(status.i)

  if (length(time.i) != n || length(status.i) != n) stop("time.i and status.i must match length of time.p.")
  if (anyNA(time.i) || anyNA(status.i)) stop("Missing values not allowed in Surv_ice().")

  # Optional checks for status.i (you can pass your own allowed set)
  if (!all(status.i %in% c(0,1))) {
    stop("status.i must be in c(0,1)")
  }

  Y <- cbind(time.p = time.p, status.p = status.p, time.i = time.i, status.i = status.i)
  class(Y) <- c("Surv.ice", "matrix")
  attr(Y, "type") <- "smcmprsk"
  return(Y)
}
}
