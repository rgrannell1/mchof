\name{mcAnd}
\alias{mcAnd}
\title{mcAnd}
\usage{
  mcAnd(f, g)
}
\arguments{
  \item{f}{a function that returns a logical value, or a
  string giving the name of such a function.}

  \item{g}{a function that returns a logical value, or a
  string giving the name of such a function.}
}
\value{
  returns a logical value.
}
\description{
  mcAnd takes two functions f and g, and returns a
  function. This new function returns f(...) && g(...)
}
\examples{

h <- function (name) name == "cat"
w <- function (name) name == "dog"

cat_or_dog <- mcAnd(h, w)
cat_or_dog("rat")

# example without saving result of mcAnd to 
# intermediate function

f <- function (n) n > 5
g <- function (n) n < 10

mcAnd(f, g)(7)

## pairing mcAnd with mcSelect

mcSelect(
	mcAnd(
		function (x) x < 10,
		function (x) x == 7),
	seq_len(20)
)
}
\seealso{
  see \code{\link{mcOr}}, \code{\link{mcNot}} and
  \code{\link{mcXor}} for other logical functionals in
  mchof.
}
\keyword{mcAnd}

