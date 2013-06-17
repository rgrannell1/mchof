\name{mcAll}
\alias{mcAll}
\title{mcAll}
\usage{
  mcAll(f, x, paropts = NULL)
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
  it returns FALSE.
}
\description{
  mcAny checks if a predicate function f is true for all
  elements in the list or vector x
}
\section{Special Cases}{
  This function applies \code{f} to each element of
  \code{x}, and coerces the results to a \code{TRUE} or
  \code{FALSE} value. If an \code{NA} value is obtained
  this is then coerced to a \code{FALSE} value. This is
  usually the desired behaviour, but if the user wants
  \code{NA} values to be converted to \code{TRUE} then they
  can wrap the input function \code{f} with
  \link{mcBoolean}.

  All quantifiers return \code{NULL} when x = \code{NULL}.
  When \code{x} is another length-zero value mcAll returns
  \code{TRUE}, and mcOne and mcAny return \code{FALSE}. The
  reason that mcAll returns \code{TRUE} for length-zero
  data is for consistency with the base function
  \code{all}.
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
}
\seealso{
  Other quantifiers: \code{\link{mcAny}},
  \code{\link{mcOne}}
}
\keyword{mcAll}

