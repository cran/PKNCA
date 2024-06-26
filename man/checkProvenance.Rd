% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/provenance.R
\name{checkProvenance}
\alias{checkProvenance}
\title{Check the hash of an object to confirm its provenance.}
\usage{
checkProvenance(object)
}
\arguments{
\item{object}{The object to check provenance for}
}
\value{
\code{TRUE} if the provenance is confirmed to be consistent, \code{FALSE} if
the provenance is not consistent, or \code{NA} if provenance is not present.
}
\description{
Check the hash of an object to confirm its provenance.
}
\seealso{
\code{\link[=addProvenance]{addProvenance()}}
}
