\name{mcFilter}
\alias{mcFilter}
\alias{mcPartition}
\alias{mcReject}
\alias{mcSelect}
\title{Filter-like Higher-Order-Functions}
\usage{
  mcFilter(f, x, paropts = NULL)

  mcSelect(f, x, paropts = NULL)

  mcReject(f, x, paropts = NULL)

  mcPartition(f, x, paropts = NULL)
}
\description{
  \code{mcFilter} extracts the elements of a vector or list
  \code{x} for which the function \code{f} returns
  \code{TRUE}.

  \code{mcSelect} is an alias for \code{mcFilter}.

  \code{mcReject} extracts the elements of a vector or list
  \code{x} for which the function \code{f} returns
  \code{FALSE}.

  \code{mcPartition} returns a list of two lists/vectors; a
  list for which \code{f} returns \code{TRUE}, and a list
  for which \code{f} returns \code{FALSE}.
}
\details{
  All of these function return \code{NULL} automatically
  when \code{x} is NULL. \code{mcFilter} and
  \code{mcReject} return \code{x} when it is length-zero
  values automatically.

  \code{mcReject} also returns elements for which \code{f}
  returned \code{NA} are included, so that concatenating
  the results of \code{mcFilter} and \code{Reject} will
  give you the original set \code{x} (though probably in
  the wrong order).

  \code{mcPartition} always returns a list of two
  lists/vectors; the first list/vector contains the values
  for which \code{f} returned \code{TRUE}, the other
  contains values that returned \code{FALSE} or \code{NA}.
  When trying to partition a length-zero value a list
  containing two of that value is returned. For example,
  when \code{x} is \code{integer(0)} then

  \code{list( integer(0), integer(0) )}

  is returned.
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
\keyword{mcFilter}
\keyword{mcPartition}
\keyword{mcReject}
\keyword{mcSelect}

