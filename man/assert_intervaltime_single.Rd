% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/assertions.R
\name{assert_intervaltime_single}
\alias{assert_intervaltime_single}
\title{Assert that an interval is accurately defined as an interval, and return the
interval}
\usage{
assert_intervaltime_single(interval = NULL, start = NULL, end = NULL)
}
\arguments{
\item{interval}{Numeric vector of two numbers for the start and end time of
integration}

\item{start}{The start time of the interval}

\item{end}{The end time of the interval}
}
\value{
\code{interval} (or \code{c(start, end)})
}
\description{
Assert that an interval is accurately defined as an interval, and return the
interval
}
\keyword{Internal}
