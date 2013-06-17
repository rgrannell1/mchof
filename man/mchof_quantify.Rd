\name{mcAll}
\alias{mcAll}
\alias{mcAny}
\alias{mcOne}
\title{Higher-Order-Functions for Quantifying TRUE/FALSE cases}
\usage{
  mcAll(f, x, paropts = NULL)

  mcAny(f, x, paropts = NULL)

  mcOne(f, x, paropts = NULL)
}
\arguments{
  \item{f}{a unary function that returns a boolean value,
  or a string giving the name of such a function.}

  \item{x}{a list or vector.}

  \item{paropts}{a list of parameters to be handed to
  mclapply (see \link{mchof}).}
}
\description{
  mcAll checks if a predicate function \code{f} is true for
  all elements in the list or vector \code{x}.

  mcAny checks if a predicate function \code{f} is true for
  one or more elements in the list or vector \code{x}.

  mcOne checks if a predicate function \code{f} is true for
  exactly one element in the list or vector \code{x}
}
\details{
  These functions apply \code{f} to each element of
  \code{x}, and coerce the results to a \code{TRUE} or
  \code{FALSE} value. If an \code{NA} value is obtained it
  is coerced to \code{FALSE}. This is usually the desired
  behaviour, but if the user wants \code{NA} values to be
  converted to \code{TRUE} then they can wrap the input
  function \code{f} with \code{\link{mcBoolean}}. For
  example,

  \code{all_are_numbers = mcBoolean \%of\% function (x)
  mcAll(is.numeric, x)}

  \code{all_are_numbers} first checks whether every
  \code{x} is numeric, and then coerces this result to
  \code{TRUE} or \code{FALSE}.

  All of these functions return \code{NULL} when \code{x =
  NULL}. When \code{x} is another length-zero value
  \code{mcAll} returns \code{TRUE}, and \code{mcOne} and
  \code{mcAny} return \code{FALSE}. The reason that mcAll
  returns \code{TRUE} for length-zero data is for
  consistency with the base function \code{all}.
}
\examples{

NULL
}
\keyword{mcAll,}
\keyword{mcAny,}
\keyword{mcOne}

