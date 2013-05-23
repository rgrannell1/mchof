\name{mcUnzipWith}
\alias{mcUnzipWith}
\title{mcUnzipWith}
\usage{
  mcUnzipWith(f, x, paropts = NULL)
}
\arguments{
  \item{f}{a function that takes a single n-element list,
  or a string giving the name of such a function.}

  \item{x}{a list of lists or vectors}

  \item{paropts}{paropts a list of parameters to be handed
  to mclapply (see \link{mchof}).}
}
\description{
  mcUnzipWith is the inverse function of mcZipWith; it
  takes a list of n element lists, makes returns n lists
  and applies a function to these lists before returning
  these lists
}
\details{
  Names are dropped without warning during unzipping; named
  outputs are given in the example below
}
\examples{

}
\seealso{
  see \code{\link{mcZipWith}} for the inverse of this
  function and \code{\link{mcUnzip}} for a shorthand
  variant of this function with f set to identity.
}
\keyword{mcUnzipWith}

