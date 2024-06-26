% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/choose.intervals.R
\name{find.tau}
\alias{find.tau}
\title{Find the repeating interval within a vector of doses}
\usage{
find.tau(x, na.action = stats::na.omit, options = list(), tau.choices = NULL)
}
\arguments{
\item{x}{the vector to find the interval within}

\item{na.action}{What to do with NAs in \code{x}}

\item{options}{List of changes to the default PKNCA options (see
\code{PKNCA.options()})}

\item{tau.choices}{the intervals to look for if the doses are not all equally
spaced.}
}
\value{
A scalar indicating the repeating interval with the most repetition.
\enumerate{
\item If all values are \code{NA} then NA is returned.
\item If all values are the same, then 0 is returned.
\item If all values are equally spaced, then that spacing is
returned.
\item If one of the \code{choices} can minimize the number of
unique values, then that is returned.
\item If none of the \code{choices} can minimize the number of
unique values, then -1 is returned.
}
}
\description{
This is intended to find the interval over which x repeats by the rule
\code{unique(mod(x, interval))} is minimized.
}
\seealso{
Other Interval determination: 
\code{\link{choose.auc.intervals}()}
}
\concept{Interval determination}
