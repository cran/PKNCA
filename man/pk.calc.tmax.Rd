% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pk.calc.simple.R
\name{pk.calc.tmax}
\alias{pk.calc.tmax}
\title{Determine time of maximum observed PK concentration}
\usage{
pk.calc.tmax(conc, time, options = list(), first.tmax = NULL, check = TRUE)
}
\arguments{
\item{conc}{Concentration measured}

\item{time}{Time of concentration measurement}

\item{options}{List of changes to the default
\code{\link{PKNCA.options}} for calculations.}

\item{first.tmax}{If there is more than time that matches the
maximum concentration, should the first be considered as Tmax?  If
not, then the last is considered Tmax.}

\item{check}{Run \code{\link{check.conc.time}}?}
}
\value{
the time of the maximum concentration
}
\description{
Input restrictions are:
\enumerate{
  \item the \code{conc} and \code{time} must be the same length,
  \item the \code{time} may have no NAs,
}
\code{NA} will be returned if:
\enumerate{
  \item the length of \code{conc} and \code{time} is 0
  \item all \code{conc} is 0 or \code{NA}
}
}
