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
  This function tries preserve the formals arguments of its
  input functions in the composite output function. There
  are ways three ways in which formals might be outputted

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

  primitive functions (such as "+") are supported by mchof,
  and their arguments are processed in a similar way to
  those of closure functions.
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
\keyword{mcSmaller}

