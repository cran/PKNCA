% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pk.calc.simple.R
\name{pk.calc.cstart}
\alias{pk.calc.cstart}
\title{Determine the concentration at the beginning of the interval}
\usage{
pk.calc.cstart(conc, time, start)
}
\arguments{
\item{conc}{Observed concentrations during the interval}

\item{time}{Times of \code{conc} observations}

\item{start}{Start time of the interval}
}
\value{
The concentration when \code{time == end}.  If none
  match, then \code{NA}
}
\description{
Determine the concentration at the beginning of the interval
}
\seealso{
Other NCA parameters for concentrations during the intervals: 
\code{\link{pk.calc.clast.obs}()},
\code{\link{pk.calc.cmax}()},
\code{\link{pk.calc.ctrough}()}
}
\concept{NCA parameters for concentrations during the intervals}
