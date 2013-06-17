\name{mcZipWith}
\alias{mcUnzip}
\alias{mcUnzipWith}
\alias{mcZip}
\alias{mcZipWith}
\title{Higher-Order-Functions for Zipping and Unzipping Lists}
\usage{
  mcZipWith(f, ..., paropts = NULL)

  mcZip(..., paropts = NULL)

  mcUnzipWith(f, x, paropts = NULL)

  mcUnzip(x, paropts = NULL)
}
\arguments{
  \item{f}{a function that takes n arguments, or a string
  giving the name of such a function.}

  \item{...}{several lists or vectors.}

  \item{paropts}{a list of parameters to be handed to
  mclapply (see \link{mchof}).}
}
\value{
  returns the result of mapping f over a list of n element
  lists.
}
\description{
  mcZipWith takes n lists or vectors, generates a list of n
  element lists, and returns the result of mapping f over
  each n element list.

  mcZip takes n lists/vectors, and generates a list of n
  element lists. It is a special case of mcZipWith

  mcUnzipWith is the inverse function of mcZipWith; it
  takes a list of n element lists, makes returns n lists
  and applies a function to these lists before returning
  these lists.

  mcUnzip is the inverse function of mcZip; it takes a list
  of n element lists/vectors, and returns n lists. It is a
  special case of mcUnzipWith
}
\details{
  list names are dropped without warning during zipping; an
  example below shows how to add names to the output list.
  NULL elements in x are automatically removed from x. The
  empty list is not removed in order act as a 'zero' to
  preserve useful structural identities.

  the input lists are assumed to be of equal length; if
  they are not excess elements are discarded without
  warning.

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

NULL
}
\keyword{mcUnzip,}
\keyword{mcUnzipWith}
\keyword{mcZip,}
\keyword{mcZipWith,}

