\name{mcNot}
\alias{mcNot}
\title{mcNot}
\usage{
  mcNot(f)
}
\arguments{
  \item{f}{a function that returns a logical value, or a
  string giving the name of such a function.}
}
\value{
  returns a function that returns false when f returns
  true, true when f returns false and na when f returns na.
}
\description{
  mcNot takes a function f, and negates its logical output.
}
\examples{

is_ten <- function (n) n == 10
not_ten <- mcNot(is_ten)
not_ten(10)

# cancel out mcNot with a second mcNot

mcNot(mcNot( function () TRUE ))()

# select the non null elements from a list

mcSelect(
	mcNot('is.null'),
	list(1, NULL, 2,3,4)
)


}
\seealso{
  see \code{\link{mcAnd}}, \code{\link{mcOr}}, and
  \code{\link{mcXor}} for other logical functionals in
  mchof
}
\keyword{mcNot}

