\name{mcFlip}
\alias{mcExplode}
\alias{mcFlip}
\alias{mcImplode}
\alias{mcJumble}
\alias{mcParameters}
\alias{mcPartial}
\title{Functions for Modifying Function Parameters}
\usage{
  mcFlip(f)

  mcJumble(f, x)

  mcParameters(f, x)

  mcExplode(f)

  mcImplode(f)

  mcPartial(f, ...)
}
\arguments{
  \item{f}{a function, or a string giving the name of a
  function.}

  \item{x}{a vector that is the same length as the formal
  parameters of x and contains all the integers 1, 2, ...,
  length(arguments). This vector determines how the
  arguments of f will be permuted.}
}
\description{
  \code{mcFlip} mcFlip takes a function \code{f}, and
  returns \code{f} with its parameters reversed.

  \code{mcJumble} takes a function \code{f}, and returns
  \code{f} with its parameters permuted as defined by
  \code{x}.
}
\details{
  the vector x determines how the formals of f will be
  permuted. For example, if \code{x = c(3, 1, 2), f =
  function (a, b, c) sum(a, b, c)} the new formals are c,
  a, b; the 3rd element in formals, the 1st element in
  formals and the 2nd element in formals.
}
\examples{

NULL
}

