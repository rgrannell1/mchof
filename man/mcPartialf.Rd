\name{mcPartialf}
\alias{mcPartialf}
\title{mcPartialf}
\usage{
  mcPartialf(f, ...)
}
\arguments{
  \item{f}{a function with no variadic (...) or primitive
  arguments, or a string giving the name of such a
  function.}

  \item{...}{name = value pairs to apply to f.}
}
\value{
  returns a partially applied version of f, with modified
  formals.
}
\description{
  mcPartialf ttransforms a function that takes multiple
  arguments into a function that takes less arguments by
  partially applying the function with the arguments
  supplied.This function differs from mcPartial in that it
  alters the formals of its return function rather than
  using ellipses (...).
}
\details{
  mcPartialf is superficially similar to mcPartial, but it
  differs in the kind of function that it returns.
  mcPartialf modifies the formals parameters of f, rather
  than just setting the formals of f to ellipses (...).
  This allows other functionals to modify the formals of an
  mcPartialf'd function, but not one modified by mcPartial.

  The downside to this implementation is that functions
  with variadic arguments (...) or primitive arguments
  (base function such as plus) do not work well with
  mcPartialf.
}
\examples{

}
\seealso{
  see \code{\link{mcPartial}} for a partial application
  function that works with variadic and primitive formal
  parameters.
}
\keyword{mcPartialf}

