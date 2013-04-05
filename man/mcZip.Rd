\name{mcZip}
\alias{mcZip}
\title{mcZip}
\usage{
  mcZip(x, paropts = NULL)
}
\arguments{
  \item{x}{a list of lists}

  \item{paropts}{a list of parameters to be handed to
  \code{mclapply} (see details)}
}
\description{
  mcZip takes n lists/vectors, and generates a list of n
  element lists. It is a special case of mcZipWith
}
\details{
  mcZip discards excess elements without warning: for
  example list (1, 2), list (3, 4, 5) becomes list (list(1,
  3), list(2, 4)).
}
\examples{
# zip a name list and an id list into name:id pairs
mcZip (list( list('Jack', 'Jane', 'Joe'), list(1, 2, 3)))
}
\author{
  Ryan Grannell
}
\seealso{
  see \code{\link{mclapply}} for more details about the
  parallel backend being employed, \code{\link{mcUnzip}}
  for the inverse of this function, and
  \code{\link{mcZipWith}} for a more general version of
  this function.
}
\keyword{mcZip}

