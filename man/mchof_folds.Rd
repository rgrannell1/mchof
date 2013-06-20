\name{mcFold}
\alias{mcFold}
\alias{mcFoldl}
\alias{mcFoldr}
\alias{mcReduce}
\alias{mcReducel}
\alias{mcReducer}
\title{Fold-like Higher-Order-Functions}
\usage{
  mcFold(f, z, x, paropts = NULL)

  mcReduce(f, x, paropts = NULL)

  mcFoldl(f, z, x)

  mcFoldr(f, z, x)

  mcReducel(f, x)

  mcReducer(f, x)
}
\arguments{
  \item{f}{a binary function that takes two of "a thing"
  and returns one of a "thing".}

  \item{x}{a list or vector.}

  \item{z}{an initial value to be prepended to x}

  \item{paropts}{a list of parameters to be handed to
  mclapply (see \link{mchof}).}
}
\description{
  \code{mcFold} applies an associative binary function
  \code{f} to a list or vector \code{x}, returning a single
  value. If x is length zero then an initial value is
  returned.

  \code{mcReduce} applies an associative binary function
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

# sum the vector 1:10 using each function in turn
#=#=#=#=#=#=#=#=#=#=#=#=#=#=##=#=#=#=#

mcFoldl('+', 0, 1:10)
mcFoldr('+', 0, 1:10)
mcReduce('+', 1:10)
mcReducl('+', 1:10)
mcReducer('+', 1:10)

# define as_list using a fold
#=#=#=#=#=#=#=#=#=#=#=#=#=#=##=#=#=#=#

as_list <- function (x) {

	mcFoldl(
		function (acc, new) {
			c(acc, as.list(new))
		},
		list(), x)
}

# sum the vector 1:10 using each function in turn
#=#=#=#=#=#=#=#=#=#=#=#=#=#=##=#=#=#=#

mcFoldl('+', 0, 1:10)
mcFoldr('+', 0, 1:10)
mcReduce('+', 1:10)
mcReducl('+', 1:10)
mcReducer('+', 1:10)

# define as_list using a fold
#=#=#=#=#=#=#=#=#=#=#=#=#=#=##=#=#=#=#

as_list <- function (x) {

	mcFoldl(
		function (acc, new) {
			c(acc, as.list(new))
		},
		list(), x)
}
}

