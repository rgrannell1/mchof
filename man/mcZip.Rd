\name{mcZip}
\alias{mcZip}
\title{mcZip}
\usage{
  mcZip(x, paropts = NULL)
}
\arguments{
  \item{x}{a list of lists}

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
  mcZip discards excess elements without warning: for
  example list (1, 2), list (3, 4, 5) becomes list (list(1,
  3), list(2, 4)).
}
\examples{

# zip two vectors into a list of 2-element lists

mcZip(
	list(
		1:5, letters[1:5]	
))

# zip three lists togerther

mcZip(
	list(
		list('R', 'Matlab', 'SAS'),
		list('language', 'language', 'languge'),
		list('GNU', 'not GNU', 'not GNU')
))

# zip two lists, with some elements discarded

mcZip(
	list(
		1:10,
		letters[1:4]
))
}
\author{
  Ryan Grannell
}
\seealso{
  see \code{\link{mcUnzip}} for the inverse of this
  function, and \code{\link{mcZipWith}} for a more general
  version of this function.
}
\keyword{mcZip}

