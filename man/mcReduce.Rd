\name{mcReduce}
\alias{mcReduce}
\title{mcReduce}
\usage{
  mcReduce(f, x, init, right, accumulate,
    is.associative = FALSE, paropts = NULL)
}
\arguments{
  \item{f}{a binary function}

  \item{x}{a vector or list}

  \item{init}{an \code{R} object of the same kind of the
  elements of x}

  \item{right}{a boolean value. Should the function be
  reduced from left to right, or from right to left?}

  \item{accumulate}{a boolean value. Should each
  intermediate step in the Reduce be returned?}

  \item{is.associative}{a boolean value. Is f an
  associative function (i.e) (x1 f x2) f x3 == x1 (f x2 f
  x3)}

  \item{paropts}{paropts a list of parameters to be handed
  to mclapply (see details and \code{\link{mclapply}})}
}
\description{
  mcReduce
}

