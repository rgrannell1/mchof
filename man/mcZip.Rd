\name{mcZip}
\alias{mcZip}
\title{mcZip}
\usage{
  mcZip(..., paropts = NULL)
}
\arguments{
  \item{...}{several lists or vectors.}

  \item{paropts}{a list of parameters to be handed to
  mclapply (see \link{mchof}).}
}
\value{
  returns a list of n element lists.
}
\description{
  mcZip takes n lists/vectors, and generates a list of n
  element lists. It is a special case of mcZipWith
}
\details{
  list names are dropped without warning during zipping; an
  example below shows how to add names to the output list.
  NULL elements in x are automatically removed from x.  The
  empty list is not removed in order act as a 'zero' to
  preserve useful structural identities.

  the input lists are assumed to be of equal length; if
  they are not excess elements are discarded without
  warning.
}
\examples{

# zip two vectors into a list of 2-element lists

mcZip(1:5, letters[1:5])

# zip three lists togerther

mcZip(
	list('R', 'Matlab', 'SAS'),
	list('language', 'language', 'languge'),
	list('GNU', 'not GNU', 'not GNU')
)

# zip two lists, with some elements discarded

mcZip(1:10,letters[1:4])

}
\seealso{
  see \code{\link{mcUnzip}} for the inverse of this
  function, and \code{\link{mcZipWith}} for a more general
  version of this function.
}
\keyword{mcZip}

