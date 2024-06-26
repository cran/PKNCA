#' Determine time at or above a set value
#'
#' Interpolation is performed aligning with `PKNCA.options("auc.method")`.
#' Extrapolation outside of the measured times is not yet implemented.  The
#' `method` may be changed by giving a named `method` argument, as well.
#'
#' For `'lin up/log down'`, if `clast` is above `conc_above` and there are
#' concentrations BLQ after that, linear down is used to extrapolate to the BLQ
#' concentration (equivalent to AUCall).
#'
#' @inheritParams pk.calc.auxc
#' @param conc_above The concentration to be above
#' @param ... Extra arguments.  Currently, the only extra argument that is used
#'   is `method` as described in the details section.
#' @returns the time above the given concentration
#' @export
pk.calc.time_above <- function(conc, time,
                               conc_above,
                               #method=NULL,
                               ...,
                               options=list(),
                               check=TRUE) {
  arglist <- list(...)
  method <- PKNCA.choose.option(name="auc.method", value=arglist$method, options=options)
  if (missing(conc)) {
    stop("conc must be given")
  }
  if (missing(time)) {
    stop("time must be given")
  }
  stopifnot("conc_above must be a scalar"=length(conc_above) == 1)
  stopifnot("conc_above must not be NA"=!is.na(conc_above))
  stopifnot("conc_above must be numeric"=is.numeric(conc_above))
  if (check) {
    assert_conc_time(conc = conc, time = time)
  }
  # Only keep evaluable rows
  data <- data.frame(conc=conc, time=time)[!is.na(conc), , drop=FALSE]
  # Determine intervals where both values are above, the first value is above,
  # or the second value is above
  data$above <- data$conc >= conc_above
  mask_both <- data$above[-1] & data$above[-nrow(data)]
  mask_first <- !data$above[-1] & data$above[-nrow(data)]
  mask_second <- data$above[-1] & !data$above[-nrow(data)]
  conc1 <- data$conc[-nrow(data)]
  conc2 <- data$conc[-1]
  time1 <- data$time[-nrow(data)]
  time2 <- data$time[-1]
  # Calculations
  if (nrow(data) < 2) {
    ret <- structure(NA_real_, exclude="Too few measured concentrations to assess time_above")
  } else if (method %in% 'lin up/log down') {
    linear_up <- conc2 > conc1 | conc2 == conc1
    linear_down <- conc2 == 0
    log_down <- !linear_down & conc2 > 0
    ret <-
      sum((time2 - time1)[mask_both]) +
      # log down
      sum(
        ((log(conc_above) - log(conc1))/(log(conc2) - log(conc1))*(time2 - time1))[mask_first & log_down]
      ) +
      # linear down
      sum(
        ((conc_above - conc1)/(conc2 - conc1)*(time2 - time1))[mask_first & linear_down]
      ) +
      # linear up
      sum(
        ((conc2 - conc_above)/(conc2 - conc1)*(time2 - time1))[mask_second & linear_up]
      )
  } else if (method %in% 'linear') {
    ret <-
      sum((time2 - time1)[mask_both]) +
      # linear down
      sum(
        ((conc_above - conc1)/(conc2 - conc1)*(time2 - time1))[mask_first]
      ) +
      # linear up
      sum(
        ((conc2 - conc_above)/(conc2 - conc1)*(time2 - time1))[mask_second]
      )
  } else {
    # Should be caught by the method assignment above
    stop("Invalid 'method', please report this as a bug: ", method) # nocov
  }
  ret
}
# Add the column to the interval specification
add.interval.col("time_above",
                 FUN="pk.calc.time_above",
                 values=c(FALSE, TRUE),
                 unit_type="time",
                 pretty_name="Time above Concentration",
                 desc="Time above a given concentration")
PKNCA.set.summary(
  name="time_above",
  description="arithmetic mean and standard deviation",
  point=business.mean,
  spread=business.sd
)
