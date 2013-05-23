\name{mcUnzip}
\alias{mcUnzip}
\title{mcUnzip}
\usage{
  mcUnzip(x, paropts = NULL)
}
\arguments{
  \item{x}{a list of lists or vectors}

  \item{paropts}{paropts a list of parameters to be handed
  to mclapply (see \link{mchof}).}
}
\description{
  mcUnzip is the inverse function of mcZip; it takes a list
  of n element vectors, and returns n lists. mcUnzip (mcZip
  (x)) |-> x
}
\details{
  mcUnzip discards excess elements, as with mcZip.
}
\examples{

# unzip two lists

mcUnzip(
	list(
		c('hat', 'dance'),
		c('may', 'pole'),
		c('silicon', 'chip')
))
}
\author{
  Ryan Grannell
}
\seealso{
  see \code{\link{mcZip}} for the inverse of this function,
  \code{\link{mcUnzipWith}} for a more general version of
  this function.
}
\keyword{mcUnzip}

