#' Add the imputation column to the intervals, if it is not already there
#'
#' @param object The PKNCAdata object to impute data within
#' @return The PKNCAdata object with an impute column added to the intervals (if
#'   it is not already there) and the object$impute set to that column name
#' @keywords internal
add_impute_to_intervals <- function(object) {
  if (is.na(object$impute)) {
    rlang::abort(
      message = "add_impute_to_intervals cannot have an NA imputation defined",
      class = "pknca_add_impute_to_intervals_NA"
    )
  }
  # Do nothing if the impute column is already in the data
  if (!(object$impute %in% names(object$intervals))) {
    impute_col <-
      if (!("impute" %in% names(object$intervals))) {
        "impute"
      } else {
        paste0(max(grep(x = names(object$intervals), "^impute", value = TRUE)), "X")
      }
    object$intervals[[impute_col]] <- object$impute
    object$impute <- impute_col
  }
  object
}

#' Methods for imputation of data with PKNCA
#' @name PKNCA_impute_method
#' @return A data.frame with one column named conc with imputed concentrations
#'   and one column named time with the times.
NULL

#' @describeIn PKNCA_impute_method Add a new concentration of 0 at the start
#'   time, even if a nonzero concentration exists at that time (usually used
#'   with single-dose data)
#' @inheritParams pk.calc.auxc
#' @param start,end The start and end of the interval
#' @param ... ignored
#' @export
PKNCA_impute_method_start_conc0 <- function(conc, time, start=0, ..., options = list()) {
  ret <- data.frame(conc = conc, time = time)
  mask_start <- time %in% start
  if (any(mask_start)) {
    ret$conc[mask_start] <- 0
  } else {
    ret <- rbind(ret, data.frame(time = start, conc = 0))
    ret <- ret[order(ret$time), ]
  }
  ret
}

#' @describeIn PKNCA_impute_method Add a new concentration of the minimum during
#'   the interval at the start time (usually used with multiple-dose data)
#' @export
PKNCA_impute_method_start_cmin <- function(conc, time, start, end, ..., options = list()) {
  ret <- data.frame(conc = conc, time = time)
  mask_start <- time %in% start
  if (!any(mask_start)) {
    all_concs <- conc[start <= time & time <= end]
    if (!all(is.na(all_concs))) {
      cmin <- min(all_concs, na.rm = TRUE)
      ret <- rbind(ret, data.frame(time = start, conc = cmin))
      ret <- ret[order(ret$time), ]
    }
  }
  ret
}

#' @describeIn PKNCA_impute_method Shift a predose concentration to become the
#'   time zero concentration (only if a time zero concentration does not exist)
#' @param max_shift The maximum amount of time to shift a concentration forward
#'   (defaults to 5\% of the interval duration, i.e. \code{0.05*(end - start)})
#' @export
PKNCA_impute_method_start_predose <- function(conc, time, start, end, ..., max_shift = NA_real_, options = list()) {
  ret <- data.frame(conc = conc, time = time)
  if (is.na(max_shift)) {
    max_shift <- 0.05 * (end - start)
  }
  mask_zero <- time %in% start
  if (!any(mask_zero)) {
    mask_predose <- time < start
    if (any(mask_predose)) {
      time_predose <- max(time[mask_predose])
      if ((-time_predose) <= max_shift) {
        mask_predose_change <- time == time_predose
        ret$time[mask_predose_change] <- start
      }
    }
  }
  ret
}

#' Separate out a vector of PKNCA imputation methods into a list of functions
#'
#' An error will be raised if the functions are not found.
#'
#' This function is not for use by users of PKNCA.
#'
#' @param x The character vector of PKNCA imputation method functions (without
#'   the \code{PKNCA_impute_method_} part)
#' @return A list of character vectors of functions to run.
#' @keywords Internal
PKNCA_impute_fun_list <- function(x) {
  if (all(is.na(x))) {
    x <- rep(NA_character_, length(x))
  }
  ret <- strsplit(x = x, split = "[, ]+", perl = TRUE)
  mask_none <- vapply(X = ret, FUN = length, FUN.VALUE = 1L) == 0
  ret[mask_none] <- NA_character_
  ret <- lapply(X = ret, FUN = PKNCA_impute_fun_list_paste)
  # Confirm that the functions exist and are functions
  # Sort will ensure that the results are not NA
  all_funs <- sort(unlist(ret))
  bad_fun <- character()
  for (idx in seq_along(all_funs)) {
    found_fun <- utils::getAnywhere(all_funs[[idx]])
    if (length(found_fun$objs) == 0) {
      bad_fun <- c(bad_fun, all_funs[[idx]])
    } else if (!is.function(found_fun$objs[[1]])) {
      bad_fun <- c(bad_fun, all_funs[[idx]])
    }
  }
  if (length(bad_fun) > 0) {
    stop(
      "The following imputation functions were not found: ",
      paste(bad_fun, collapse = ", ")
    )
  }
  ret
}

# A helper for PKNCA_impute_fun_list that pastes PKNCA_impute_method_ to the
# beginning of everything but NA
PKNCA_impute_fun_list_paste <- function(x) {
  mask_paste <- !is.na(x) & !startsWith(x, "PKNCA_impute_method_")
  if (any(mask_paste)) {
    x[mask_paste] <- paste0("PKNCA_impute_method_", x[mask_paste])
  }
  x
}
