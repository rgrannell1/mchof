\name{mcLarger}
\alias{mcDivide}
\alias{mcEqual}
\alias{mcLarger}
\alias{mcMinus}
\alias{mcMultiply}
\alias{mcNotEqual}
\alias{mcPlus}
\alias{mcSmaller}
\title{Higher-Order-Functions for Functional Arithmetic}
\usage{
  mcLarger(f, g)

  mcSmaller(f, g)

  mcPlus(f, g)

  mcMinus(f, g)

  mcEqual(f, g)

  mcNotEqual(f, g)

  mcMultiply(f, g)

  mcDivide(f, g)
}
\arguments{
  \item{f}{a function that returns a value, or a string
  giving the name of such a function.}

  \item{g}{a function that returns a value, or a string
  giving the name of such a function.}
}
\description{
  \code{mcLarger} takes two functions \code{f} and
  \code{g}, and returns a composite function. this
  composite function returns \code{f(...) > g(...)}

  \code{mcSmaller} takes two functions \code{f} and
  \code{g}, and returns a composite function. this
  composite function returns \code{f(...) < g(...)} '
  \code{mcPlus} takes two functions \code{f} and \code{g},
  and returns a composite function. this composite function
  returns \code{f(...) + g(...)}

  \code{mcMinus} takes two functions \code{f} and \code{g},
  and returns a composite function. this composite function
  returns \code{f(...) - g(...)}

  \code{mcEqual} takes two functions \code{f} and \code{g},
  and returns a composite function. this composite function
  returns \code{f(...) == g(...)}

  \code{mcNotEqual} takes two functions \code{f} and
  \code{g}, and returns a composite function. this
  composite function returns \code{f(...) != g(...)}

  \code{mcMultiply} takes two functions \code{f} and
  \code{g}, and returns a composite function. this
  composite function returns \code{f(...) * g(...)}

  \code{mcDivide} takes two functions \code{f} and
  \code{g}, and returns a composite function. this
  composite function returns \code{f(...) / g(...)}
}
\details{
  1
}
\examples{

}

