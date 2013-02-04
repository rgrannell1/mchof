\name{mcFilter}
\alias{mcFilter}
\title{mcFilter}
\usage{
  mcFilter(f, x, paropts = NULL)
}
\arguments{
  \item{f,}{a unary function that returns either
  \code{TRUE} or \code{FALSE}}

  \item{x,}{a vector}

  \item{paropts,}{a list of parameters to be handed to
  mclapply (see \code{\link{mclapply}})}
}
\description{
  mcFilter extracts the elements of a vector for which the
  function \code{f} returns true, in parallel
}
\details{
  give all the details
}
\examples{
# remove NA values from a vector
p <- function(x) !is.na(x)
mcFilter(p, c(3,2,6,NA, 2))
}
\seealso{
  a list of related and relevant functions
}

