\name{mcAll}
\alias{mcAll}
\alias{mcAny}
\alias{mcOne}
\title{Quantifier Functionals}
\usage{
  mcAll(f, x, paropts = NULL)

  mcAny(f, x, paropts = NULL)

  mcOne(f, x, paropts = NULL)
}
\arguments{
  \item{f}{a unary function that returns a boolean value,
  or a string giving the name of such a function.}

  \item{x}{a list or vector.}

  \item{paropts}{a list of parameters to be handed to
  mclapply (see \link{mchof}).}
}
\description{
  mcAll checks if a predicate function \code{f} is true for
  all elements in the list or vector \code{x}.

  mcAny checks if a predicate function \code{f} is true for
  one or more elements in the list or vector \code{x}.

  mcOne checks if a predicate function \code{f} is true for
  exactly one element in the list or vector \code{x}
}
\details{
  These functions apply \code{f} to each element of
  \code{x}, and coerce the results to a \code{TRUE} or
  \code{FALSE} value. If an \code{NA} value is obtained it
  is coerced to \code{FALSE}. This is usually the desired
  behaviour, but if the user wants \code{NA} values to be
  converted to \code{TRUE} then they can wrap the input
  function \code{f} with \code{\link{mcBoolean}}. For
  example,

  \code{all_are_numbers = mcBoolean \%of\% function (x)
  mcAll(is.numeric, x)}

  \code{all_are_numbers} first checks whether every
  \code{x} is numeric, and then coerces this result to
  \code{TRUE} or \code{FALSE}.

  All of these functions return \code{NULL} when \code{x =
  NULL}. When \code{x} is another length-zero value
  \code{mcAll} returns \code{TRUE}, and \code{mcOne} and
  \code{mcAny} return \code{FALSE}. The reason that mcAll
  returns \code{TRUE} for length-zero data is for
  consistency with the base function \code{all}.
}
\examples{

# check that all the numbers in a list are odd

is_odd <- function (n) (n \%\% 2) == 0
mcAll(is_odd, c(2, 4, 6, 7, 8))

# check that every string is an email

is_email <- function (str) {
	grepl('.+@.+[.].+', str)
}

mcAll(
	is_email,
	c('me@gmail.com', 'you@yahoo.com', 'we@google.ie')	
)

# check that the mean value of every list is 
# larger than two
	
mcAll(
	function (x) mean(x) > 0,
	list(
		c(1,2,3),
		c(4,4,4),
		c(-10, 6)
))


# check that all the numbers in a list are odd

is_odd <- function (n) (n \%\% 2) == 0
mcAny(is_odd, c(2, 4, 6, 7, 8))

# check that every string is an email

is_email <- function (str) {
	grepl('.+@.+[.].+', str)
}

mcAny(
	is_email,
	c('me@gmail.com', 'you@yahoo.com', 'we@google.ie')	
)

# check that the mean value of every list is 
# larger than two

mcAny(
	function (x) mean(x) > 0,
	list(
		1:3,
		6:2,
		-2:6
))


# check that all the numbers in a list are odd

is_odd <- function (n) (n \%\% 2) == 0
mcOne(is_odd, c(2, 4, 6, 7, 8))

# check that every string is an email

is_email <- function (str) {
	grepl('.+@.+[.].+', str)
}

mcOne(
	is_email,
	c('me@gmail.com', 'you@yahoo.com', 'we@google.ie')	
)

# check that the mean value of every list is 
# larger than two

mcOne(
	function (x) mean(x) > 0,
	list(
		c(1,2,3),
		c(4,4,4),
		c(-10, 6)
))



}
\keyword{mcAll,}
\keyword{mcAny,}
\keyword{mcOne}

