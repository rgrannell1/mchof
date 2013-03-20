\name{mcUnzip}
\alias{mcUnzip}
\title{mcUnzip}
\usage{
  mcUnzip(..., paropts)
}
\arguments{
  \item{...}{a list of vectors/lists of length > 0}

  \item{paropts}{a list of parameters to be handed to
  \code{mclapply} (see details)}
}
\description{
  mcUnzip is the inverse function of mcZip. mcUnzip (mcZip
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
  and \code{\link{mclapply}} for more details about the
  parallel backend being employed.
}
\keyword{mcUnzip}

