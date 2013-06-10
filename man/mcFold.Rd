\name{mcFold}
\alias{mcFold}
\title{mcFold}
\usage{
  mcFold(f, first, x, paropts = NULL)
}
\arguments{
  \item{f}{a binary function that takes two of "a thing"
  and returns one of a "thing".}

  \item{first}{an initial value to be prepended to x}

  \item{x}{a vector or list}

  \item{paropts}{a list of parameters to be handed to
  mclapply (see \link{mchof}).}
}
\value{
  returns the result of x1 f x2 f x3 f x4 f ... xn, the
  value of which is dependent on the function f, and the
  contents of x. when x is NULL, NULL is automatically
  returned, as with other mchof functions. when x is a
  length-zero input such as integer(0) or list()
  \code{first} is returned.
}
\description{
  mcFold applies an associative binary function to a list,
  returning a single value. The difference between mcFold &
  mcReduce is that an initial value can be supplied to
  mcFold.
}
\details{
  mcFold can be used as a parallel alternative to Fold if
  and only if the function f is associative; that is

  \code{(a f b) f c == a f (b f c)},

  where a, b or c are values that f takes. For example,
  plus is an associative binary operator, since \code{(a +
  b) + c == a + (b + c)} for any number a, b or c. Minus
  does not have this property, so it is not suitable for
  use with mcFold. Only associative binary functions can be
  folded or reduced in parallel.

  Formally the combination of an associative binary
  operator, an identity element (first) and a set (x) is
  known as a monoid; the function f has a type signature of
  [A] -> [A] -> [A]. A likely source of errors when using
  mcFold or mcReduce is using a function without this type
  signature (ie. a function that takes two of a thing, and
  returns one of a thing).

  it is often useful to use the identity of f as first, as
  it can make it possible to simplify f. For example, lists
  have an identity element of list() when concatenated, and
  integers have an identity of 0 under addition. This is
  shown below in the example programs given.
}
\examples{

# bind a list of vectors into a column

mcFold(
	function (a, b) cbind(a, b),
	c(),
	list(
		1:4, 2:5,
		3:6, 4:7
))

# get the 4th power of a matrix 

fourth_power <- function (X) {
	I <- diag(1, nrow(X), ncol(X))
	
	mcFold(
		f = function (a, b) a \%*\% b,
		first = I,
		x = rep(list(X), 4))
}
fourth_power( t(matrix(1:4, 2, 2)) )


}

