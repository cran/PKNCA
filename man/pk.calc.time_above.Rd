% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/time.above.R
\name{pk.calc.time_above}
\alias{pk.calc.time_above}
\title{Determine time at or above a set value}
\usage{
pk.calc.time_above(conc, time, conc_above, ..., options = list(), check = TRUE)
}
\arguments{
\item{conc}{Concentration measured}

\item{time}{Time of concentration measurement (must be monotonically
increasing and the same length as the concentration data)}

\item{conc_above}{The concentration to be above}

\item{...}{Extra arguments.  Currently, the only extra argument that is used
is \code{method} as described in the details section.}

\item{options}{List of changes to the default \code{\link{PKNCA.options}} for
calculations.}

\item{check}{Run \code{\link{check.conc.time}}, \code{\link{clean.conc.blq}},
and \code{\link{clean.conc.na}}?}
}
\value{
the time above the given concentration
}
\description{
Interpolation is performed aligning with \code{PKNCA.options("auc.method")}.
Extrapolation outside of the measured times is not yet implemented.  The
\code{method} may be changed by giving a named \code{method} argument, as
well.
}
\details{
For \code{'lin up/log down'}, if \code{clast} is above \code{conc_above} and
there are concentrations BLQ after that, linear down is used to extrapolate
to the BLQ concentration (equivalent to AUCall).
}
