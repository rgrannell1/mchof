\name{mcPartialf}
\alias{mcPartialf}
\title{mcPartial}
\usage{
  mcPartialf(f, ...)
}
\arguments{
  \item{f}{a function, or a string giving the name of a
  function.}

  \item{...}{name = value pairs to apply to f.}
}
\value{
  returns a partially applied function with ellipsis (...)
  formals
}
\description{
  mcPartial transforms a function that takes multiple
  arguments into a function that takes less arguments by
  partially applying the function with the arguments
  supplied.
}
\details{
  mcPartial returns a partially applied function; an
  example of a partially applied function is

  f(x,y) 2x + y -> f(x) 2x + 2
}
\examples{

sentence <- mcCurry(paste, collapse = " ")
sentence(c("this", "is", "a", "curried", "function"))

# increment a list 1..10 with curried plus function

plus <- function (a, b) {
	# + doesn't actually have formal arguments, so 
	# this is necessary
	
	a + b
}
increment <- mcCurry(plus, a = 1)

Map("increment", 1:10)

## find every email in a vector

is_email <- mcCurry(grepl, pattern = "[^@]+[@][^.]+[.].+")

mcSelect(is_email, c("hi@gmail.com", "cran@cran.cran", "not email"))
}
\seealso{
  see \code{\link{mcPartialf}} for a partial application
  function that operates on the formals of f and performs
  input validation.
}
\keyword{mcPartial}

