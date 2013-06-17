\name{mcPosition}
\alias{mcFind}
\alias{mcPosition}
\title{Higher-Order-Functions for Finding Values}
\usage{
  mcPosition(f, x, right = FALSE, paropts = NULL)

  mcFind(f, x, right = FALSE, paropts = NULL)
}
\arguments{
  \item{f}{a unary function that returns a boolean value,
  or a string giving the name of such a function.}

  \item{x}{a list or vector.}

  \item{right}{a boolean value. Should \code{x} be searched
  starting from the right? Defaults to \code{FALSE}.}

  \item{paropts}{a list of parameters to be handed to
  mclapply (see \link{mchof}).}
}
\description{
  \code{mcPosition} returns the index of the first (or
  last) position in \code{x} whose value matches a
  predicate function \code{f}.

  \code{mcFind} returns the value of the first (or last)
  element of \code{x} that meets the predicate f.
}
\examples{

# find the first value larger than 10

mcPosition(
	function (n) n > 10,
	c(1,2,11,3,4,-2))

# check for a match from the right

mcPosition(
	function (n) n > 10,
	c(1,2,11,3,4,-2),
	right = TRUE)

# find the first string matching a regular expression

mcPosition(
	function (n) grepl('yahoo', n),
	c('greg@gmail.com', 'bob@yahoo.com', 'phil@yahoo.ie')
)

# return the first value larger than 10

mcFind(
	function (n) n > 10,
	c(1,2,11,3,4,-2))

# return the first match from the right

mcFind(
	function (n) n > 10,
	c(1,2,11,3,4,-2),
	right = TRUE)

# return the first string matching a regular expression

mcFind(
	function (n) grepl('yahoo', n),
	c('greg@gmail.com', 'bob@yahoo.com', 'phil@yahoo.ie')
)

}
\keyword{mcFind}
\keyword{mcPosition}

