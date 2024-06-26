% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class-PKNCAresults.R
\name{as.data.frame.PKNCAresults}
\alias{as.data.frame.PKNCAresults}
\title{Extract the parameter results from a PKNCAresults and return them as a
data.frame.}
\usage{
\method{as.data.frame}{PKNCAresults}(
  x,
  ...,
  out_format = c("long", "wide"),
  filter_requested = FALSE,
  filter_excluded = FALSE,
  out.format = deprecated()
)
}
\arguments{
\item{x}{The object to extract results from}

\item{...}{Ignored (for compatibility with generic \code{\link[=as.data.frame]{as.data.frame()}})}

\item{out_format}{Should the output be 'long' (default) or 'wide'?}

\item{filter_requested}{Only return rows with parameters that were
specifically requested?}

\item{filter_excluded}{Should excluded values be removed?}

\item{out.format}{Deprecated in favor of \code{out_format}}
}
\value{
A data.frame (or usually a tibble) of results
}
\description{
Extract the parameter results from a PKNCAresults and return them as a
data.frame.
}
