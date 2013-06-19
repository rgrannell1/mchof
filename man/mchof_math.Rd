\name{mcLarger}
\alias{mcDivide}
\alias{mcEqual}
\alias{mcLarger}
\alias{mcMax}
\alias{mcMinus}
\alias{mcMultiply}
\alias{mcNotEqual}
\alias{mcPlus}
\alias{mcSmaller}
\title{Higher-Order-Functions for Functional Arithmetic}
\usage{
  mcLarger(f, g)

  mcSmaller(f, g)

  mcPlus(f, g)

  mcMinus(f, g)

  mcEqual(f, g)

  mcNotEqual(f, g)

  mcMultiply(f, g)

  mcDivide(f, g)

  mcMax(f, g)
}
\arguments{
  \item{\code{f}}{a function that returns an appropriate
  value (a number for every function but \code{mcEqual} and
  \code{mcNotEqual}), or a string giving the name of such a
  function.}

  \item{g}{a function with the same restrictions as
  \code{f}}
}
\description{
  1

  \code{mcLarger} takes two functions \code{f} and
  \code{g}, and returns a composite function. this
  composite function returns \code{f(...) > g(...)}

  \code{mcSmaller}, \code{mcPlus}, \code{mcMinus},
  \code{mcEqual}, \code{mcNotEqual}, \code{mcMultiply}, and
  \code{mcDivide} are equivalent wrappers for their
  respective operators.
}
\details{
  1
}
\examples{

# get the values of x
# for which one polynomial is larger than polynomial

mcSelect(
	mcLarger(
		function (x) x^2 - 2 * x + 3,
		function (x) 2^x^2 - 10
	),
	-10:10
)

# is composition of linear functions equivelant to 
# multiplication?

f <- function (x) x * 2
g <- function (x) x * 3

mcAll(
	mcEqual(
		f \%of\% g,
		function (x) x * 2 * 3
	),
	0:100
)

# partition a list into values for which
# sin + cos is bigger than sin + sin

mcPartition(
	mcLarger(
		mcPlus(sin, cos),
		mcPlus(sin, sin)),
	1:100
)

# find the intersection of two lines, if there
# is one

mcSelect(
	mcEqual(
		function (n) 2*n + 4,
		function (n) 3*n + 12
	),
	-100:100
)

# create a function 
# sin(x) - cos(cos(x))

mcMinus(sin, cos \%of\% cos)

}

