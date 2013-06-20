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

# find all primes in 1...15
#=#=#=#=#=#=#=#=#=#=#=#=#=#=##=#=#=#=#

is_prime <- function (n) {
    if (n == 1 || n == 2) FALSE else {
        !any( n \%\% (2:(n - 1)) == 0)
    }
}

mcSelect(is_prime, 1:15) #or
mcReject(mcNot(is_prime), 1:15)

# find the largest prime factor of 2023
#=#=#=#=#=#=#=#=#=#=#=#=#=#=##=#=#=#=#

mcReduce(max,
	mcSelect(
		mcAnd(is_prime, function (n) (2023 \%\% n) == 0) ,
		1:2023) 
)

# find solutions to 2*x + 4*y < 100
#=#=#=#=#=#=#=#=#=#=#=#=#=#=##=#=#=#=#

mcSelect(
    mcImplode(
		function (x, y) {
			2*x + 4*y < 100
		}
	),
	mcZip(
		sample(1:100, size = 100),
		sample(1:100, size = 100)
	)
)

# Project Euler 8: find the greatest product of 5-consecutive digits
# in a 1000-digit number
#=#=#=#=#=#=#=#=#=#=#=#=#=#=##=#=#=#=#

# use random data instead of the given number

number <- sample(0:9, size = 1000, replace = TRUE)

indices <- mcReduce(
	function (a, b) {
		if (prod(number[a]) > prod(number[b])) a else b
	},
	mcZipWith(
		"+",
		rep(list(1:5), 995), 1:1000)
)

# the mcZipWith step generates the indices 
# [1,..., 5], [2,..., 6], ... [995, ..., 1000]

number[indices]

# partition a vector into good cops and other kinds of cop
#=#=#=#=#=#=#=#=#=#=#=#=#=#=##=#=#=#=#

mcPartition(
	function (cop) {
		cop == "good cop"
	},
	c("good cop", "bad cop", "good cop", "bad cop", "it's a fair cop!")
)

}
\keyword{mcFilter}
\keyword{mcPartition}
\keyword{mcReject}
\keyword{mcSelect}

