\name{mcPartition}
\alias{mcPartition}
\title{mcPartition}
\usage{
  mcPartition(f, x, paropts = NULL)
}
\arguments{
  \item{f}{a unary function that returns a boolean value}

  \item{x}{a vector or list}

  \item{paropts}{a list of parameters to be handed to
  \code{mclapply} (see details)}
}
\description{
  mcPartition returns a list of two lists; a list for which
  a predicate returns true, and a list for which a
  predicate returns false
}
\details{
  NA values obtained during logical filtering are assumed
  to be FALSE, as with other functions in this package. The
  user can modify this behaviour by making sure the
  argument f returns TRUE is a value is NA under coersion.
}
\examples{
# partition a set into even and odd numbers
mcPartition ( function (x) x \%\% 2, 1:10, paropts = list(mc.cores = 2))
# divide a set of combinations into two based on a predicate
mcPartition(
	f = function(pair){
		val <- sum(unlist(pair))
		if (val > 8) TRUE else FALSE
	},
	x = apply(combn(8, 3), 2, list),
	paropts = list(mc.cores = 2))
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

