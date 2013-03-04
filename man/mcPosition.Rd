\name{mcPosition}
\alias{mcPosition}
\title{mcPosition}
\usage{
  mcPosition(f, x, right = FALSE, paropts = NULL)
}
\arguments{
  \item{f}{a unary function that returns either \code{TRUE}
  or \code{FALSE}}

  \item{x}{a vector or list}

  \item{right}{a boolean value. Should the first
  \code{TRUE} or last \code{FALSE} element matching
  \code{f} be returned? Defaults to \code{FALSE}}

  \item{paropts}{a list of parameters to be handed to
  mclapply (see details and \code{\link{mclapply}})}
}
\description{
  Returns the index of the first (or last) position in a
  vector or list matching a predicate function, in
  parallel.
}
\details{
  mcPosition returns integer(0) if no match is found, in
  much the same way that which(0 == 1) returns integer(0)
}
\examples{
# find the index of the first position of the first non-NA value in a vector
mcPosition(function(x) !is.na(x), c(10, NA, 11:20), paropts = list(mc.cores = 2))
# get the index of the first value larger than five from the rightmost index of a vector
mcPosition(function(x) x > 5, 1:10, right=TRUE, paropts=list(mc.cores = 2))
}
\seealso{
  see \code{\link{Position}} for the non-parallel
  equivelant of this function, \code{\link{mclapply}} for
  more details about the parallel backend being employed.
}

