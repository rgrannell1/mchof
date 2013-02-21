\name{mcPosition}
\alias{mcPosition}
\title{mcPosition}
\usage{
  mcPosition(f, x, right = FALSE, paropts = NULL)
}
\arguments{
  \item{f}{a unary function that returns either \code{TRUE}
  or \code{FALSE}}

  \item{x}{a vector}

  \item{right}{a boolean value. Should the first
  \code{TRUE} or last \code{FALSE} element matching
  \code{f} be returned? Defaults to \code{FALSE}}

  \item{nomatch}{the variable that is returned if no
  elements such that \code{f(element) = TRUE} are found in
  \code{x}. Defaults to \code{NA}}

  \item{paropts}{a list of parameters to be handed to
  mclapply (see details and \code{\link{mclapply}})}
}
\description{
  Returns the index of the first (or last) position in a
  vector or list matching a predicate function, in
  parallel.
}
\details{
  many details
}
\examples{
mcPosition(function(x) !is.null(x), 10:20, paropts = list(mc.cores = 2))
mcPosition(function(x) x > 5, 1:10, right=TRUE, paropts=list(mc.cores = 2))
}
\seealso{
  see \code{\link{Position}} for the non-parallel
  equivelant of this function, \code{\link{mclapply}} for
  more details about the parallel backend being employed.
}

