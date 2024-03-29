% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AIC.list.R
\name{get.best.model}
\alias{get.best.model}
\title{Extract the best model from a list of models using AIC.list.}
\usage{
get.best.model(object, ...)
}
\arguments{
\item{object}{the list of models}

\item{\dots}{Parameters passed to AIC.list}
}
\value{
The model which is assessed as best.  If more than one are
equal, the first is chosen.
}
\description{
Extract the best model from a list of models using AIC.list.
}
\seealso{
\code{\link{AIC.list}}
}
