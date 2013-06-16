\name{mcSmaller}
\alias{mcSmaller}
\title{mcSmaller}
\usage{
  mcSmaller(f, g)
}
\arguments{
  \item{f}{a function that returns a number or vector of
  numbers, or a string giving the name of such a function.}

  \item{g}{a function that returns a number or vector of
  numbers, or a string giving the name of such a function.}
}
\value{
  returns a logical value

  #'
}
\description{
  mcSmaller takes two functions f and g, and returns a
  function. This new function returns f(...) < g(...)
}
\details{
  This function tries to preserve the formals arguments of
  its input functions in the composite output function.
  There are ways three ways in which the composite function
  might be outputted:

  1, If the function \code{f} and \code{g} have the same
  parameter names and the same default arguments - in the
  same order - then the output function will preserve the
  parameters and default arguments of \code{f}/\code{g}.

  2, If the function \code{f} and \code{g} have the same
  parameter names - in the same order - but have different
  default arguments, then the output function will preserve
  only the parameters of \code{f}/\code{g}.

  3, If the parameters of \code{f} and \code{g} differ, or
  their order is shuffled then the output function uses
  ellipses as parameters.

  primitive functions (such as \code{max}) are supported by
  mchof, and their arguments are processed in a similar way
  to those of closures (standard R functions).
}
\examples{

# is the first function smaller than the second when n = 10?

f <- function (n) 2*n
g <- function (n) 3*n - 4
first_bigger <- mcSmaller(f, g)
first_bigger(10)

# select the values of x that make the first 
# function smaller than the second

mcSelect(
	mcSmaller(
		function (x) x^2 - 2,
		function (x) x^3 - 10),
	x = 1:10)



}
\seealso{
  Other math functions: \code{\link{mcDivide}},
  \code{\link{mcEqual}}, \code{\link{mcLarger}},
  \code{\link{mcMinus}}, \code{\link{mcMultiply}},
  \code{\link{mcNotEqual}}, \code{\link{mcPlus}}
}
\keyword{mcSmaller}

