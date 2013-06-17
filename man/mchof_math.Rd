\name{mcLarger}
\alias{mcDivide}
\alias{mcEqual}
\alias{mcLarger}
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
}
\arguments{
  \item{f}{a function that returns a value, or a string
  giving the name of such a function.}

  \item{g}{a function that returns a value, or a string
  giving the name of such a function.}
}
\description{
  \code{mcLarger} takes two functions \code{f} and
  \code{g}, and returns a composite function. this
  composite function returns \code{f(...) > g(...)}

  \code{mcSmaller} takes two functions \code{f} and
  \code{g}, and returns a composite function. this
  composite function returns \code{f(...) < g(...)} '
  \code{mcPlus} takes two functions \code{f} and \code{g},
  and returns a composite function. this composite function
  returns \code{f(...) + g(...)}

  \code{mcMinus} takes two functions \code{f} and \code{g},
  and returns a composite function. this composite function
  returns \code{f(...) - g(...)}

  \code{mcEqual} takes two functions \code{f} and \code{g},
  and returns a composite function. this composite function
  returns \code{f(...) == g(...)}

  \code{mcNotEqual} takes two functions \code{f} and
  \code{g}, and returns a composite function. this
  composite function returns \code{f(...) != g(...)}

  \code{mcMultiply} takes two functions \code{f} and
  \code{g}, and returns a composite function. this
  composite function returns \code{f(...) * g(...)}

  \code{mcDivide} takes two functions \code{f} and
  \code{g}, and returns a composite function. this
  composite function returns \code{f(...) / g(...)}
}
\details{
  1
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


f <- function (x) 2
g <- function (x) 2 * x

f_equals_g <- mcEqual(f, g)
f_equals_g(1)

# invoking the function without intermediate steps

mcEqual(
	function (n) n + n,
	function (n) n^2) (1)


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


# function is vectorised

f <- function (x) x^2
g <- function (x) x^3

square_minus_cube <- mcMinus(f, g)
square_minus_cube(1:10)

# invoking the function without intermediate steps

mcMinus(
	function (n) n + n,
	function (n) n^2) (1:5)


# function is vectorised

f <- function (x) x^2
g <- function (x) x^3

square_by_cube <- mcMultiply(f, g)
square_by_cube(1:10)

# invoking the function without intermediate steps

mcMultiply(
	function (n) n + n,
	function (n) n^2) (1:5)


# function is vectorised

f <- function (x) x^2
g <- function (x) x^3

square_plus_cube <- mcPlus(f, g)
square_plus_cube(1:10)

# invoking the function without intermediate steps

mcPlus(
	function (n) n + n,
	function (n) n^2) (1:5)


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

