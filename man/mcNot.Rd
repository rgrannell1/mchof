\name{mcNot}
\alias{mcNot}
\title{mcAnd}
\usage{
  mcNot(f)
}
\arguments{
  \item{f}{a function that returns a logical value, or a
  string giving the name of such a function.}
}
\value{
  returns a function that returns false when f returns
  true, true when f returns false and na when f returns na
}
\description{
  mcAnd takes a function f, and negates its logical output
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
  see \code{\link{mcAnd}}, \code{\link{mcOr}}, and
  \code{\link{mcXor}}
}
\keyword{mcAnd}

