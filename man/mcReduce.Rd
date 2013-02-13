\name{mcReduce}
\alias{mcReduce}
\title{mcReduce}
\usage{
  mcReduce(f, x, paropts = NULL)
}
\arguments{
  \item{f}{a binary function}

  \item{x}{a vector or list}

  \item{paropts}{paropts a list of parameters to be handed
  to mclapply (see details and \code{\link{mclapply}})}
}
\description{
  \code{mcReduce} can be used as a parallel replacement for
  a subclass of problems that can be solved with Reduce;
  parallelising reduce is only possible when the function f
  is associative; that is

  \code{(x1 f x2) f x3} is equivalent to \code{x1 f (x2 f
  x3)}. In practicality this means that the order in which
  the Reduce is carried out isn't important, so several
  tasks can be carried out in parallel.

  A binary function with associativity is the plus
  operator; \code{1 + (2 + 3) == (1 + 2) + 3}.  Subtraction
  does not have this property \code{1 - (2 - 3) != (1 - 2)
  - 3}, so it should not be used as a binary function for
  \code{mcReduce}
}
\author{
  Ryan Grannell
}

