#' Times relative to an event (typically dosing)
#' 
#' @details 
#' 
#' @param time_event A vector of times for events
#' @param time_obs A vector of times for observations
#' @param units Passed to `base::as.numeric.difftime()`
#' @return A data.frame with columns for:
#' \itemize{
#'   \item{time_after}
#'   \item{time_before}
#'   \item{event_number_after}
#'   \item{event_number_before}
#' }
#' 
#' `time_after` and `time_before` are calculated where 
#' @export
time_calc <- function(time_event, time_obs, units=NULL)
  UseMethod("actual_time_calc")

time_calc.numeric <- function(time_event, time_obs, units=NULL) {
  if (!("numeric" %in% class(time_obs))) {
    stop("Both `time_event` and `time_obs` must be the same class (POSIXt).")
  }
}

time_calc.POSIXt <- function(time_event, time_obs, units=NULL) {
  if (is.null(units)) {
    stop("`units` must be provided.")
  }
  if (!("POSIXt" %in% class(time_obs))) {
    stop("Both `time_event` and `time_obs` must be the same class (POSIXt).")
  }
  first_event <- min(time_event, na.rm=TRUE)
}

time_calc.difftime <- function(time_event, time_obs, units=NULL) {
  if (is.null(units)) {
    stop("`units` must be provided.")
  }
  if (!("difftime" %in% class(time_obs))) {
    stop("Both `time_event` and `time_obs` must be the same class (difftime).")
  }
  
}
