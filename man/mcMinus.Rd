\name{mcMinus}
\alias{mcMinus}
\title{mcMinus}
\usage{
  mcMinus(f, g)
}
\arguments{
  \item{f}{a function that returns a number or vector of
  numbers, or a string giving the name of such a function.}

  \item{g}{a function that returns a number or vector of
  numbers, or a string giving the name of such a function.}
}
\value{
  returns a number or vector of numbers.
}
\description{
  mcMinus takes two functions f and g, and returns a
  function. This new function returns f(...) - g(...)
}
\examples{

# function is vectorised

f <- function (x) x^2
g <- function (x) x^3

square_minus_cube <- mcMinus(f, g)
square_minus_cube(1:10)

# invoking the function without intermediate steps

mcMinus(
	function (n) n + n,
	function (n) n^2) (1:5)


}
\keyword{mcMinus}

