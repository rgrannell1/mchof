\name{mcFilter}
\alias{mcFilter}
\alias{mcSelect}
\title{mcFilter}
\usage{
  mcFilter(f, x, paropts = NULL)

  mcSelect(f, x, paropts = NULL)

  This function applies \code{f} to each element of
    \code{x}, and coerces the results to a \code{TRUE} or
    \code{FALSE} value. If an \code{NA} value is obtained
    this is then coerced to a \code{FALSE} value. This is
    usually the desired behaviour, but if the user wants
    \code{NA} values to be converted to \code{TRUE} then
    they can wrap the input function \code{f} with
    \link{mcBoolean}.
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
\section{Special Cases}{
  when x is a length-zero value such as NULL or list(),
  that value is automatically returned.
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
  Other filters: \code{\link{mcPartition}},
  \code{\link{mcReject}}
}
\keyword{mcFilter}
\keyword{mcSelect}

