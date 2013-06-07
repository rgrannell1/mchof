\name{mcUnzip}
\alias{mcUnzip}
\title{mcUnzip}
\usage{
  mcUnzip(x, paropts = NULL)
}
\arguments{
  \item{x}{a list of lists or vectors}

  \item{paropts}{a list of parameters to be handed to
  mclapply (see \link{mchof}).}
}
\description{
  mcUnzip is the inverse function of mcZip; it takes a list
  of n element vectors, and returns n lists. mcUnzip (mcZip
  (x)) |-> x
}
\details{
  list names are dropped without warning during unzipping;
  an example below shows how to add names to the output
  list. NULL elements in x are automatically removed from
  x. The empty list is not removed in order act as a 'zero'
  to preserve useful structural identities.

  the input lists are assumed to be of equal length; if
  they are not excess elements are discarded without
  warning.
}
\examples{

# unzip three lists of 2-elements into two 3-element lists

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

