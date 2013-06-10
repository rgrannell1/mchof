\name{mcOne}
\alias{mcOne}
\title{mcOne}
\usage{
  mcOne(f, x, paropts = NULL)
}
\arguments{
  \item{f}{a unary function that returns a boolean value,
  or a string giving the name of such a function.}

  \item{x}{a list or vector.}

  \item{paropts}{a list of parameters to be handed to
  mclapply (see \link{mchof}).}
}
\value{
  returns TRUE if f is true for one element in x, otherwise
  it returns FALSE. If x is NULL, NULL is returned. If x is
  an empty list or vector FALSE is returned.
}
\description{
  mcOne checks if a predicate function f is true for
  exactly one element in the list or vector x
}
\details{
  NA's obtained while applying f to x will be assumed to be
  FALSE. the user can sidestep this behaviour easily, if
  necessary (see \link{mchof}).
}
\examples{

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
\keyword{mcOne}

