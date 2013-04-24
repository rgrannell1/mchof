\name{mcReduce}
\alias{mcReduce}
\title{mcReduce}
\usage{
  mcReduce(f, x, paropts = NULL)
}
\arguments{
  \item{x}{a vector or list}

  \item{paropts}{paropts a list of parameters to be handed
  to mclapply (see details and \code{\link{mclapply}})}

  \item{f}{a binary function}
}
\description{
  mcReduce applies an associative binary function to a
  list, returning a single value
}
\details{
  \code{mcReduce} can be used as a parallel replacement for
  a subclass of problems that can be solved with
  \code{Reduce} ; parallelising \code{Reduce} is only
  possible when the function f is associative; in infix
  notation that is \code{(x1 f x2) f x3} is equivalent to
  \code{x1 f (x2 f x3)}. In practicality this means that
  the order in which the reduce is carried out isn't
  important, so several tasks can be carried out in
  parallel.

  A binary function with associativity is the plus
  operator; \code{1 + (2 + 3) == (1 + 2) + 3}.  Subtraction
  does not have this property \code{1 - (2 - 3) != (1 - 2)
  - 3}, so it should not be used as a binary function for
  \code{mcReduce}
}
\examples{
mcReduce(get('+'), 1:10)
mcReduce(rbind, list(c(1, 2), c(3, 4), c(5, 6)))
}
\seealso{
  \code{\link{Reduce}}
}
\keyword{mcReduce}

