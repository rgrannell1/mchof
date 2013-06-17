\name{mcPosition}
\alias{mcFind}
\alias{mcPosition}
\title{Higher-Order-Functions for Finding Values}
\usage{
  mcPosition(f, x, right = FALSE, paropts = NULL)

  mcFind(f, x, right = FALSE, paropts = NULL)
}
\arguments{
  \item{f}{a unary function that returns a boolean value,
  or a string giving the name of such a function.}

  \item{x}{a list or vector.}

  \item{right}{a boolean value. Should \code{x} be searched
  starting from the right? Defaults to \code{FALSE}.}

  \item{paropts}{a list of parameters to be handed to
  mclapply (see \link{mchof}).}
}
\description{
  \code{mcPosition} returns the index of the first (or
  last) position in \code{x} whose value matches a
  predicate function \code{f}.

  \code{mcFind} returns the value of the first (or last)
  element of \code{x} that meets the predicate f.
}
\examples{

NULL
}
\keyword{mcFind}
\keyword{mcPosition}

