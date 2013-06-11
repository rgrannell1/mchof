\name{mcCurryf}
\alias{mcCurryf}
\title{mcCurryf}
\usage{
  mcCurryf(f, ...)
}
\arguments{
  \item{f}{a function with no variadic (...) or primitive
  arguments, or a string giving the name of such a
  function.}

  \item{...}{}
}
\value{
  returns a function
}
\description{
  mcCurryf is a form of currying involving more rigorous
  validation of input arguments, at the cost of not
  supporting variadic functions.
}
\examples{

NULL
}
\keyword{mcCurryf}

