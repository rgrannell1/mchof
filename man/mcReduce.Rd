\name{mcReduce}
\alias{mcReduce}
\title{mcReduce}
\usage{
  mcReduce(f, x, paropts = NULL)
}
\arguments{
  \item{f}{a binary function that takes two of "a thing"
  and returns one of a "thing".}

  \item{x}{a list or vector.}

  \item{paropts}{a list of parameters to be handed to
  mclapply (see \link{mchof}).}
}
\value{
  returns the result of x1 f x2 f x3 f x4 f ... xn, the
  value of which is dependent on the function f, and the
  contents of x. Returns a length-zero or length-one value
  of x as is.
}
\description{
  mcReduce applies an associative binary function to a
  list, returning a single value
}
\details{
  mcReduce can be used as a parallel alternative to Reduce
  if and only if the function f is associative; that is

  \code{(a f b) f c == a f (b f c)},

  where a, b or c are values that f takes. For example,
  plus is an associative binary operator, since

  \code{(a + b) + c == a + (b + c)}

  for any number a, b or c. Minus does not have this
  property, so it is not suitable for use with mcFold. Only
  associative binary functions can be folded or reduced in
  parallel.

  When x only has one element it is returned immediately,
  as there is no way to apply a binary function to a
  length-one list.

  A likely source of errors when using mcFold or mcReduce
  is using a function without the type signature [A] -> [A]
  -> [A] (ie. a function that takes two of a thing, and
  returns one of a thing).
}
\examples{

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
\seealso{
  \code{\link{Reduce}}
}
\keyword{mcReduce}

