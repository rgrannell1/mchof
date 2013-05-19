\name{mcUnzip}
\alias{mcUnzip}
\title{mcUnzip}
\usage{
  mcUnzip(x, paropts = NULL)
}
\arguments{
  \item{x}{a list of lists}

  \item{paropts}{a list of parameters to be handed to
  \code{mclapply} (see details)}
}
\description{
  mcUnzip is the inverse function of mcZip; it takes a list
  of n element vectors, and returns n lists. mcUnzip (mcZip
  (x)) |-> x
}
\details{
  mcUnzip discards excess elements, as with mcZip.
}
\author{
  Ryan Grannell
}
\seealso{
  see \code{\link{mcZip}} for the inverse of this function,
  \code{\link{mcUnzipWith}} for a more general version of
  this function and \code{\link{mclapply}} for more details
  about the parallel backend being employed.
}
\keyword{mcUnzip}

