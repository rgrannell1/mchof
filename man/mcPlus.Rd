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
\keyword{mcPlus}

