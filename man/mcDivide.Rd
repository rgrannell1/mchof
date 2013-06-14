\name{mcDivide}
\alias{mcDivide}
\title{mcDivide}
\usage{
  mcDivide(f, g)
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
  mcDivide takes two functions f and g, and returns a
  function. This new function returns f(...) / g(...)
}
\details{
  This function works hard to try preserve the formals
  arguments of its input functions in the composite output
  function. There are ways in which formals might be
  outputted

  1, If the function f and g have the same parameter names
  and the same default arguments - in the same order - then
  the output function will preserve the parameters and
  default arguments of f/g

  2, If the function f and g have the same parameter names
  - in the same order - but have different default
  arguments, then the output function will preserve only
  the parameters of f/g

  3, If the parameters of f and g differ, or their order is
  shuffled then the output function uses ellipses for as
  parameters.

  if any of the input function are primitive (such as '+')
  then the output function uses ellipses for parameters.
}
\examples{

# function is vectorised

f <- function (x) x^2
g <- function (x) x^3

square_over_cube <- mcDivide(f, g)
square_over_cube(1:10)

# invoking the function without intermediate steps

mcDivide(
	function (n) n + n,
	function (n) n^2) (1:5)
}
\keyword{mcDivide}

