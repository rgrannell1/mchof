\name{mcZipWith}
\alias{mcZipWith}
\title{mcZipWith}
\usage{
  mcZipWith(f, ..., paropts = NULL)
}
\arguments{
  \item{f}{a function that takes a single n-element list}

  \item{...}{a list of vectors/lists of length > 0}

  \item{paropts}{a list of parameters to be handed to
  \code{mclapply} (see details)}
}
\description{
  mcZipWith takes n lists/vectors, generates a list of n
  element lists, and returns the result of mapping f over
  this new list.
}
\details{
  mcZipWith discards excess elements without warning: for
  example list (1, 2), list (3, 4, 5) becomes list (f(
  list(1, 3) ), f( list(2, 4) )).
}
\author{
  Ryan Grannell
}
\seealso{
  see \code{\link{mclapply}} for more details about the
  parallel backend being employed.
}
\keyword{mcZipWith}

