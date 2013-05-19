\name{mcUnzipWith}
\alias{mcUnzipWith}
\title{mcUnzipWith}
\usage{
  mcUnzipWith(f, x, paropts = NULL)
}
\arguments{
  \item{f}{a function that takes a single n-element list}

  \item{x}{a list of lists}

  \item{paropts}{a list of parameters to be handed to
  \code{mclapply} (see details)}
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
\author{
  Ryan Grannell
}
\seealso{
  see \code{\link{mclapply}} for more details about the
  parallel backend being employed, \code{\link{mcZipWith}}
  for the inverse of this function and
  \code{\link{mcUnzip}} for a variant of this function.
}
\keyword{mcUnzipWith}

