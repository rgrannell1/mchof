\name{mcOne}
\alias{mcOne}
\title{mcOne}
\usage{
  mcOne(f, x, paropts = NULL)
}
\arguments{
  \item{f}{a unary function that returns a boolean value,
  or a string giving the name of a function.}

  \item{x}{a list or vector. Vectors are converted to lists
  internally.}

  \item{paropts}{paropts a list of parameters to be handed
  to mclapply (see \link{mchof}).}
}
\value{
  returns TRUE if f is true for one element in x, otherwise
  it returns FALSE. If x is NULL, NULL is returned. If x is
  an empty list an empty list is returned.
}
\description{
  mcOne checks if a predicate function f is true for
  exactly one element in the list or vector x
}
\details{
  NA's obtained while applying f to x will be assumed to be
  FALSE. the user can sidestep this behaviour easily, if
  necessary (see \link{mchof}).
}
\keyword{mcOne}

