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

  \item{...}{name = value pairs to curry f with.}
}
\value{
  returns a partially applied version of f, with modified
  formals.
}
\description{
  mcCurryf transforms a function that takes multiple
  arguments into a function that takes less arguments by
  partially applying the function. This function differs
  from mcCurry in that it also alters the formals of its
  return function.
}
\details{
  mcCurryf is superficially similar to mcCurry, but it
  differs in the kind of function that it returns. mcCurryf
  modifies the formals parameters of f, rather than just
  setting the formals of f to ellipses (...). This allows
  other functionals to modify the formals of an mcCurryf'd
  function, but not one modified by mcCurry.

  The downside to this implementation is that functions
  with variadic arguments (...) or primitive arguments
  (base function such as plus) do not work well with
  mcCurryf.
}
\examples{

}
\seealso{
  see \code{\link{mcCurry}} for a currying function that
  works with variadic and primitive formal parameters.
}
\keyword{mcCurryf}

