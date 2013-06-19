\name{mcFold}
\alias{mcFold}
\alias{mcReduce}
\title{Fold-like Higher-Order-Functions}
\usage{
  mcFold(f, first, x, paropts = NULL)

  mcReduce(f, x, paropts = NULL)
}
\arguments{
  \item{f}{a binary function that takes two of "a thing"
  and returns one of a "thing".}

  \item{x}{a list or vector.}

  \item{first}{an initial value to be prepended to x}

  \item{paropts}{a list of parameters to be handed to
  mclapply (see \link{mchof}).}
}
\description{
  \code{mcFold} applies an associative binary function
  \code{f} to a list or vector \code{x}, returning a single
  value. If x is length zero then an initial value is
  returned.

  \code{mcFold} applies an associative binary function
  \code{f} to a list or vector \code{x}, returning a single
  value.
}
\details{
  this function can be used as a parallel alternative to
  foldl or reducel if and only if the function f is
  associative; that is

  \code{(a f b) f c == a f (b f c)},

  where a, b or c are values that f takes. For example,
  plus is an associative binary operator, since

  \code{(a + b) + c == a + (b + c)}

  for any number a, b or c. Minus does not have this
  property, so it is not suitable for use with mcFold. Only
  associative binary functions can be folded or reduced in
  parallel.

  Formally the combination of an associative binary
  operator, an identity element for that operator and a set
  (x) is known as a monoid; the function \code{f} has a
  type signature of [A] -> [A] -> [A]. A likely source of
  errors when using \code{mcFold} or \code{mcReduce} is
  using a function without this type signature (ie. a
  function that takes two of a thing, and returns one of a
  thing).

  with \code{mcFold} it useful to use the identity of
  \code{f} as first, as it can make it possible to simplify
  \code{f}. For example, lists have an identity element of
  \code{list()} when concatenated, and integers have an
  identity of 0 under addition.
}
\section{Special Cases}{
  when x is NULL, NULL is automatically returned (since
  NULL falls throught all mchof functions without being
  interperated as meaningful data). If x is a length-zero
  value such as list() or integer(0) then \code{mcFold}
  returns \code{first}, and \code{mcReduce} automatically
  returns \code{x}.

  \code{mcReduce} returnes length-one values because a
  binary function cannot be applied to a single value, so
  the value is presumed to be already fully "reduced".
}
\examples{

# find all sexy primes below 1000

mcFold













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


# reduce integer operations over a list

mcReduce('+', 1:10)
mcReduce('*', 1:6)

# find the maximum value in a vector

mcReduce(max, sample(1:10))

# find the maximum value in a list, with 
# a custom max function

max_matrix <- function (A, B) {

	size_one <- prod(dim(A)) * prod(A)
	size_two <- prod(dim(B)) * prod(B)
	
	if (size_one > size_two) A else B 
}

mcReduce(
	max_matrix,
	list(
		matrix(1:6, 2, 3),
		matrix(1:4, 2, 2),
		matrix(1:16,4, 4)
))

# find all sexy primes below 1000

mcFold













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


# reduce integer operations over a list

mcReduce('+', 1:10)
mcReduce('*', 1:6)

# find the maximum value in a vector

mcReduce(max, sample(1:10))

# find the maximum value in a list, with 
# a custom max function

max_matrix <- function (A, B) {

	size_one <- prod(dim(A)) * prod(A)
	size_two <- prod(dim(B)) * prod(B)
	
	if (size_one > size_two) A else B 
}

mcReduce(
	max_matrix,
	list(
		matrix(1:6, 2, 3),
		matrix(1:4, 2, 2),
		matrix(1:16,4, 4)
))
}

