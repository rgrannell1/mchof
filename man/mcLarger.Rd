\name{mcLarger}
\alias{mcLarger}
\title{mcLarger}
\usage{
  mcLarger(f, g)
}
\arguments{
  \item{f}{a function that returns a number or vector of
  numbers, or a string giving the name of such a function.}

  \item{g}{a function that returns a number or vector of
  numbers, or a string giving the name of such a function.}
}
\value{
  returns a logical value
}
\description{
  mcLarger takes two functions f and g, and returns a
  function. This new function returns f(...) > g(...)
}
\examples{

# is the first function larger than the second when n = 10?

f <- function (n) 2*n
g <- function (n) 3*n - 4
first_bigger <- mcLarger(f, g)
first_bigger(10)

# select the values of x that make the first 
# function larger than the second

mcSelect(
	mcLarger(
		function (x) x^2 - 2,
		function (x) x^3 - 10),
	x = 1:10)



}
\keyword{mcLarger}

