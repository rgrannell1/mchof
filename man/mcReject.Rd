\name{mcReject}
\alias{mcReject}
\title{mcReject}
\usage{
  mcReject(f, x, paropts = NULL)
}
\arguments{
  \item{f}{a unary function that returns a boolean value,
  or a string giving the name of such a function.}

  \item{x}{a list or vector.}

  \item{paropts}{a list of parameters to be handed to
  mclapply (see \link{mchof}).}
}
\value{
  returns a list of elements for which f returned FALSE or
  NA.
}
\description{
  mcReject extracts the elements of a vector or list for
  which the function \code{f} returns \code{FALSE}.
}
\details{
  mcReject applies f to each element of x, coerces the
  result to a logical value, and returns the values for
  which f returns FALSE.

  mcReject is more useful for filtering out NULL or NA
  values in a list that mcFilter, as is demonstrated in the
  examples below.

  elements for which f returned NA are included, so that
  concatenating the results of mcFilter and mcReject will
  give you the original set x (though unordered). The user
  can modify this behaviour by making sure the argument f
  returns TRUE is a value is NA under coersion, as
  described in \link{mchof}.
}
\examples{
# remove NA values from a vector 

mcReject(is.na, c(3,2,6,NA, 2))

# the same example, in parallel
mcReject(is.na, c(3,2,6,NA, 2, list(mc.cores = 2)))

# find all even numbers in a vector of numbers
even_ints <- function(x){
	Filter(
		f = function(y) y \%\% 2,
		x
	)
}
even_ints(c(1,2,3,4,5,6,7,8,9,10))

# a more advanced example, using an anonymous function to
# filter out combinations that don't meet a predicate 

mcReject(
	f = function(pair){
		val <- sum(unlist(pair))
		val <= 8
	}, 
	x = apply(combn(8, 3), 2, list),
	paropts = list(mc.cores = 2)
)

mcReject(
	is.null,
	list(NULL, 1, 2, 3, NULL, 4, 5)
)
}
\seealso{
  see \code{mcFilter} for a complementary function to this,
  and \code{mcPartition} for a function that combines
  mcFilter and mcReject
}

