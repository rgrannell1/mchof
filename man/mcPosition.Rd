\name{mcPosition}
\alias{mcPosition}
\title{mcPosition}
\usage{
  mcPosition(f, x, right = FALSE, paropts = NULL)
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
  returns an integer. If no match is found or x is
  length(0) integer(0) is returned. This is consistent with
  the behaviour of which(). As with other mchof function x
  = NULL returns NULL
}
\description{
  Returns the index of the first (or last) position in a
  vector or list matching a predicate function.
}
\details{
  mcPosition can be used as a functional alternative to
  \code{which}, that works well in combination with other
  functionals in base R and this library. NA's obtained
  while applying f to x will be assumed to be FALSE. the
  user can sidestep this behaviour easily, if necessary
  (see \link{mchof}).
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
}
\seealso{
  see \link{which} for an alternate indexing function, and
  \link{mcFind} for a function which returns the first (or
  last) element in a list matching a predicate.
}
\keyword{mcPosition}

