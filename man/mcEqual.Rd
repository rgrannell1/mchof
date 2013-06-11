\name{mcEqual}
\alias{mcEqual}
\title{mcEqual}
\usage{
  mcEqual(f, g)
}
\arguments{
  \item{f}{a function that returns an object that can be
  compared with ==, or a string giving the name of such a
  function.}

  \item{g}{a function that returns an object that can be
  compared with ==, or a string giving the name of such a
  function.}
}
\value{
  returns a logical value.
}
\description{
  mcEqual takes two functions f and g, and returns a
  function. This new function returns f(...) == g(...)
}
\examples{

f <- function (x) 2
g <- function (x) 2 * x

f_equals_g <- mcEqual(f, g)
f_equals_g(1)

# invoking the function without intermediate steps

mcEqual(
	function (n) n + n,
	function (n) n^2) (1)


}
\keyword{mcEqual}

