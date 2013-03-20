\name{mcPartition}
\alias{mcPartition}
\title{mcPartition}
\usage{
  mcPartition(f, x, paropts)
}
\arguments{
  \item{f}{a unary function that returns a boolean value}

  \item{x}{a vector or list}

  \item{paropts}{a list of parameters to be handed to
  \code{mclapply} (see details)}
}
\description{
  mcPartition takes n lists/vectors, generates a list of n
  element lists, and returns the result of mapping f over
  this new list.
}
\details{
  NA values obtained during logical filtering are assumed
  to be FALSE, as with other functions in this package. The
  user can modify this behaviour by making sure the
  argument f returns TRUE is a value is NA under coersion.
}
\author{
  Ryan Grannell
}
\seealso{
  see \code{\link{mclapply}} for more details about the
  parallel backend being employed, and
  \code{\link{mcFilter}} for a function that returns only
  the values for which f returns TRUE.
}
\keyword{mcPartition}

