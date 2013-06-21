\name{mcLarger}
\alias{mcDivide}
\alias{mcEqual}
\alias{mcLarger}
\alias{mcMax}
\alias{mcMin}
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

  mcMin(f, g)
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
  \code{mcLarger} takes two functions \code{f} and
  \code{g}, and returns a composite function. This
  composite function returns \code{f(...) > g(...)}

  \code{mcSmaller}, \code{mcPlus}, \code{mcMinus},
  \code{mcEqual}, \code{mcNotEqual}, \code{mcMultiply},
  \code{mcDivide}, \code{ \code{mcMax} and \code{mcMin} are
  equivalent wrappers for their respective operators.
}
\details{
  No input validation is done for the resulting function
  returned by functions in this family. If incorrect inputs
  are given then standard R error checking is performed,
  but there are no special checks in place to check the
  type or length of the arguments/outputs. This is to
  ensure that the wrappers in this family allow the
  underlying function to behave identically to their base
  equivelants.

  This family of functions is particularily useful for
  manipulating continuous mathematical functions such as
  lines, polynomials and periodic waves.Functions like
  \code{mcPlus} will output a vectorised function if both
  of its input functions are vectorised. For example,

  \code{ f = mcPlus(function (x) x^2, function (x)
  40*sin(x)) }

  \code{ f(1:3) }

  \code{ [1] 34.65884 40.37190 14.64480 }

  the output of \code{f} is vectorised because both input
  functions are vectorised. Both the input functions and
  the output function are shown below.

  \figure{math-example.png}
}
\examples{

# get the values of x
# for which one polynomial is larger than polynomial
#=#=#=#=#=#=#=#=#=#=#=#=#=#=##=#=#=#=#

mcSelect(
	mcLarger(
		function (x) x^2 - 2 * x + 3,
		function (x) 2^x^2 - 10
	),
	-10:10
)

# is composition of linear functions equivelant to 
# multiplication?
#=#=#=#=#=#=#=#=#=#=#=#=#=#=##=#=#=#=#

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
#=#=#=#=#=#=#=#=#=#=#=#=#=#=##=#=#=#=#

mcPartition(
	mcLarger(
		mcPlus(sin, cos),
		mcPlus(sin, sin)),
	1:100
)

# find the intersection of two lines, if there
# is one
#=#=#=#=#=#=#=#=#=#=#=#=#=#=##=#=#=#=#

mcSelect(
	mcEqual(
		function (n) 2*n + 4,
		function (n) 3*n + 12
	),
	-100:100
)

# create the function sin(x) - cos(cos(x))
#=#=#=#=#=#=#=#=#=#=#=#=#=#=##=#=#=#=#

mcMinus(sin, cos \%of\% cos)

}
\keyword{mcDivide}
\keyword{mcEqual}
\keyword{mcLarger}
\keyword{mcMax}
\keyword{mcMin}
\keyword{mcMinus}
\keyword{mcMultiply}
\keyword{mcNotEqual}
\keyword{mcPlus}
\keyword{mcSmaller}

