#' Clean up the time to steady-state parameters and return a data
#' frame for use by the tss calculators.
#'
#' @param conc Concentration measured
#' @param time Time of concentration measurement
#' @param subject Subject identifiers (used as a random effect in the
#' model)
#' @param treatment Treatment description (if missing, all subjects
#' are assumed to be on the same treatment)
#' @param subject.dosing Subject number for dosing
#' @param time.dosing Time of dosing
#' @param options List of changes to the default
#' \code{\link{PKNCA.options}} for calculations.
#' @param conc.blq See \code{\link{clean.conc.blq}}
#' @param conc.na See \code{\link{clean.conc.na}}
#' @param check Run \code{\link{check.conc.time}}?
#' @param \dots Discarded inputs to allow generic calls between tss
#' methods.
#' @return a data frame with columns for \code{conc}entration,
#' \code{time}, \code{subject}, and \code{treatment}.
pk.tss.data.prep <- function(conc, time, subject, treatment,
                             subject.dosing, time.dosing,
                             options=list(),
                             conc.blq=NULL,
                             conc.na=NULL,
                             check=TRUE, ...) {
  # Check inputs
  conc.blq <- PKNCA.choose.option(name="conc.blq", value=conc.blq, options=options)
  conc.na <- PKNCA.choose.option(name="conc.na", value=conc.na, options=options)
  if (check) {
    # When subject and time are given, then monotonicity tests for
    # time are not required.
    monotonic.time <- missing(subject) & missing(treatment)
    check.conc.time(conc, time, monotonic.time=monotonic.time)
  }
  if (!missing(subject.dosing) & missing(subject)) {
    stop("Cannot give subject.dosing without subject")
  }
  if (any(is.na(time.dosing))) {
    stop("time.dosing may not contain any NA values")
  }
  if (!missing(subject)) {
    if (!missing(treatment)) {
      ret <- clean.conc.blq(conc=conc, time=time, subject, treatment,
                            conc.blq=conc.blq, conc.na=conc.na,
                            check=FALSE)
    } else {
      ret <- clean.conc.blq(conc=conc, time=time, subject,
                            conc.blq=conc.blq, conc.na=conc.na,
                            check=FALSE)
    }
  } else if (missing(treatment)) {
    ret <- clean.conc.blq(conc=conc, time=time,
                          conc.blq=conc.blq, conc.na=conc.na,
                          check=FALSE)
  } else {
    ret <- clean.conc.blq(conc=conc, time=time, treatment,
                          conc.blq=conc.blq, conc.na=conc.na,
                          check=FALSE)
  }
  if (missing(subject.dosing)) {
    # Shrink the data to just the predose data
    ret <- subset(ret, time %in% time.dosing)
  } else {
    dosing <-
      data.frame(
        subject=subject.dosing,
        time=time.dosing,
        stringsAsFactors=FALSE
      )
    # Shrink the data to just the predose data (by subject)
    ret <- merge(ret, dosing)
  }
  # Clean out unnecessary parts of the output data frame
  if ("subject" %in% names(ret)) {
    if (length(unique(ret$subject)) == 1) {
      # Drop the "subject" column from single-subject data
      ret$subject <- NULL
    } else if (!is.factor(ret$subject)) {
      # Make sure that it is a factor made from a character vector because the
      # output subject numbering will come from row.names of the random
      # effects.
      ret$subject <- factor(as.character(ret$subject))
    }
  }
  if ("treatment" %in% names(ret)) {
    if (length(unique(ret$treatment)) == 1) {
      # Drop the "treatment" column from single-treatment data
      ret$treatment <- NULL
    } else if (!is.factor(ret$treatment)) {
      # Make sure that it is a factor otherwise
      ret$treatment <- factor(ret$treatment)
    }
  }
  ret
}

#' Compute the time to steady-state (tss)
#'
#' @param \dots Passed to \code{\link{pk.tss.monoexponential}} or
#' \code{\link{pk.tss.stepwise.linear}}.
#' @param check See \code{\link{pk.tss.data.prep}}
#' @param type The type of Tss to calculate, either
#' \code{stepwise.linear} or \code{monoexponential}
#' @return A data frame with columns as defined from
#' \code{pk.tss.monoexponential} and/or \code{pk.tss.stepwise.linear}.
#' @family Time to steady-state calculations
#' @export
pk.tss <- function(...,
                   type=c("monoexponential", "stepwise.linear"),
                   check=TRUE) {
  type <- match.arg(type, several.ok=TRUE)
  ret <- NA
  if ("monoexponential" %in% type) {
    ret_monoexponential <- pk.tss.monoexponential(..., check=check)
    if (identical(NA, ret)) {
      ret <- ret_monoexponential
    } else {
      stop("Bug in pk.tss where ret is set to non-NA too early.  Please report the bug with a reproducible example.") # nocov
    }
    # Set check to FALSE if it has already been checked (so that it
    # doesn't happen again in stepwise.linear)
    check <- FALSE
  }
  if ("stepwise.linear" %in% type) {
    ret_stepwise <- pk.tss.stepwise.linear(..., check=check)
    if (identical(NA, ret)) {
      ret <- ret_stepwise
    } else {
      ret <- merge(ret, ret_stepwise, all=TRUE)
    }
  }
  ret
}
