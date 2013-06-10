\name{mcXor}
\alias{mcXor}
\title{mcXor}
\usage{
  mcXor(f, g)
}
\arguments{
  \item{f}{a function that returns a logical value, or a
  string giving the name of such a function.}

  \item{g}{a function that returns a logical value, or a
  string giving the name of such a function.}
}
\value{
  returns a logical value
}
\description{
  mcXor takes two functions f and g, and returns a
  function. This new function returns \code{xor( f(...),
  g(...) )}
}
\examples{

h <- function (name) name == "cat"
w <- function (name) name == "dog"

cat_or_dog <- mcXor(h, w)
cat_or_dog("rat")

# example without saving result of mcXor to 
# intermediate function

f <- function (n) n > 5
g <- function (n) n < 10

mcXor(f, g)(7)

## pairing mcXor with mcSelect

mcSelect(
	mcXor(
		function (x) x < 10,
		function (x) x == 7),
	seq_len(20)
)
}
\seealso{
  see \code{\link{mcAnd}}, \code{\link{mcOr}}, and
  \code{\link{mcNot}}
}
\keyword{mcXor}

