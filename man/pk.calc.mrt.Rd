% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pk.calc.simple.R
\name{pk.calc.mrt}
\alias{pk.calc.mrt}
\alias{pk.calc.mrt.iv}
\title{Calculate the mean residence time (MRT) for single-dose data or linear
multiple-dose data.}
\usage{
pk.calc.mrt(auc, aumc)

pk.calc.mrt.iv(auc, aumc, duration.dose)
}
\arguments{
\item{auc}{the AUC from 0 to infinity or 0 to tau}

\item{aumc}{the AUMC from 0 to infinity or 0 to tau}

\item{duration.dose}{The duration of the dose (usually an infusion duration
for an IV infusion)}
}
\value{
the numeric value of the mean residence time
}
\description{
Calculate the mean residence time (MRT) for single-dose data or linear
multiple-dose data.
}
\details{
mrt is \code{aumc/auc - duration.dose/2} where \code{duration.dose = 0} for oral administration.
}
\section{Functions}{
\itemize{
\item \code{pk.calc.mrt.iv()}: MRT for an IV infusion

}}
\seealso{
\code{\link[=pk.calc.mrt.md]{pk.calc.mrt.md()}}
}
