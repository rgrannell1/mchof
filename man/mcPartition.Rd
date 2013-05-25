\name{mcPartition}
\alias{mcPartition}
\title{mcPartition}
\usage{
  mcPartition(f, x, paropts = NULL)
}
\arguments{
  \item{f}{a unary function that returns a boolean value,
  or a string giving the name of such a function.}

  \item{x}{a list or vector.}

  \item{paropts}{paropts a list of parameters to be handed
  to mclapply (see \link{mchof}).}
}
\value{
  returns a list of two lists; the first list contains the
  values for which f returned true, the other contains
  values that returned false or NA.
}
\description{
  mcPartition returns a list of two lists; a list for which
  a predicate returns true, and a list for which a
  predicate returns false.
}
\examples{
# partition a set into even and odd numbers

mcPartition(
	function (x) x \%\% 2,
	1:10
) 

# divide a set of combinations into 
# two based on a predicate

mcPartition(
	f = function(pair){
		val <- sum(unlist(pair))
		val > 8
	},
	x = apply(combn(8, 3), 2, list),
	paropts = list(mc.cores = 2)
)
}
\seealso{
  see \code{\link{mcReject}} for a function that returns
  the values for which f returns false or NA, and
  \code{\link{mcFilter}} for a function that returns the
  values for which f returns true.
}
\keyword{mcPartition}

