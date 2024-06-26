% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pk.calc.urine.R
\name{pk.calc.fe}
\alias{pk.calc.fe}
\title{Calculate fraction excreted (typically in urine or feces)}
\usage{
pk.calc.fe(ae, dose)
}
\arguments{
\item{ae}{The amount excreted (as a numeric scalar or vector)}

\item{dose}{The dose (as a numeric scalar or vector)}
}
\value{
The fraction of dose excreted
}
\description{
Calculate fraction excreted (typically in urine or feces)
}
\details{
fe is \code{sum(ae)/dose}

The units for \code{ae} and \code{dose} should be the same so that \code{ae/dose}
is a unitless fraction.
}
\seealso{
\code{\link[=pk.calc.ae]{pk.calc.ae()}}, \code{\link[=pk.calc.clr]{pk.calc.clr()}}
}
