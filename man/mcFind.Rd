\name{mcFind}
\alias{mcFind}
\title{mcFind}
\usage{
  mcFind(f, x, right = FALSE, paropts = NULL)
}
\arguments{
  \item{f}{a unary function that returns a boolean value,
  or a string giving the name of such a function.}

  \item{x}{a list or vector.}

  \item{right}{a boolean value. Should the first TRUE or
  last FALSE element matching f be returned? Defaults to
  FALSE.}

  \item{paropts}{a list of parameters to be handed to
  mclapply (see \link{mchof}).}
}
\value{
  returns an element from x. If x is NULL, NULL is
  returned. If x is the empty list, the empty list is
  returned.
}
\description{
  Returns the value of the first element of x that meets
  the predicate f.
}
\details{
  mcFind applies f to each element of x, coerces the result
  to a logical value, and returns the first value for which
  f returns TRUE. NA's obtained while applying f to x will
  be assumed to be FALSE. the user can sidestep this
  behaviour easily, if necessary (see \link{mchof}).
}
\examples{

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
\seealso{
  see \code{mcPosition} to return the index of the first
  element matching f.
}
\keyword{mcFind}

