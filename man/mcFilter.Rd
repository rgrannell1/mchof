\name{mcFilter}
\alias{mcFilter}
\alias{mcSelect}
\title{mcFilter}
\usage{
  mcFilter(f, x, paropts = NULL)

  mcSelect(f, x, paropts = NULL)
}
\arguments{
  \item{f}{a unary function that returns a boolean value,
  or a string giving the name of such a function.}

  \item{x}{a list or vector.}

  \item{paropts}{a list of parameters to be handed to
  mclapply (see \link{mchof}).}
}
\value{
  returns the elements of x for which f returned true. If x
  is a list and no elements returned true, returns list().
  If x is a vector and no elements returns true, returns a
  typed vector of length(0). x = NULL always returns NULL.
}
\description{
  mcFilter extracts the elements of a vector or list for
  which the function \code{f} returns \code{TRUE}.
}
\details{
  mcFilter applies f to each element of x, coerces the
  result to a logical value, and returns the values for
  which f returns TRUE. NA's obtained while applying f to x
  will be assumed to be FALSE. the user can sidestep this
  behaviour easily, if necessary (see \link{mchof}).
}
\examples{
# remove NA values from a vector 

p <- function(x) !is.na(x)
mcFilter(p, c(3,2,6,NA, 2))

# the same example, in parallel
p <- function(x) !is.na(x)
mcFilter(p, c(3,2,6,NA, 2, list(mc.cores = 2)))

# find all even numbers in a vector of numbers
even_ints <- function(x){
	Filter(
		f = function(y) !(y \%\% 2),
		x
	)
}
even_ints(c(1,2,3,4,5,6,7,8,9,10))

# a more advanced example, using an anonymous function to
# filter out combinations that don't meet a predicate 

mcFilter(
	f = function(pair){
		val <- sum(unlist(pair))
		val > 8
	}, 
	x = apply(combn(8, 3), 2, list),
	paropts = list(mc.cores = 2)
)

# remove NULL values from a list, by first transforming the
# is.null function to a !is.null function

mcFilter(
	Negate(is.null),
	list(NULL, 1, 2, 3, NULL, 4, 5)
)
}
\seealso{
  see \code{\link{mcReject}} for a counterpart to this
  function, and \code{\link{mcPartition}} for a function
  that combines mcFilter and mcReject
}
\keyword{mcFilter}
\keyword{mcSelect}

