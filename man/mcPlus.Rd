\name{mcPlus}
\alias{mcPlus}
\title{mcPlus}
\usage{
  mcPlus(f, g)
}
\arguments{
  \item{f}{a function that returns a number or vector of
  numbers, or a string giving the name of such a function.}

  \item{g}{a function that returns a number or vector of
  numbers, or a string giving the name of such a function.}
}
\value{
  returns a number or vector of numbers
}
\description{
  mcPlus takes two functions f and g, and returns a
  function. This new function returns f(...) + g(...)
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

# function is vectorised

f <- function (x) x^2
g <- function (x) x^3

square_plus_cube <- mcPlus(f, g)
square_plus_cube(1:10)

# invoking the function without intermediate steps

mcPlus(
	function (n) n + n,
	function (n) n^2) (1:5)

}
\seealso{
  Other math functions: \code{\link{mcDivide}},
  \code{\link{mcEqual}}, \code{\link{mcLarger}},
  \code{\link{mcMinus}}, \code{\link{mcMultiply}},
  \code{\link{mcNotEqual}}, \code{\link{mcSmaller}}
}
\keyword{mcPlus}

