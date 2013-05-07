\name{mcAny}
\alias{mcAny}
\title{mcAny}
\usage{
  mcAny(f, x, paropts = NULL)
}
\arguments{
  \item{f}{a unary function that returns a boolean value}

  \item{x}{a list or vector}

  \item{paropts}{paropts a list of parameters to be handed
  to mclapply (see details and \code{\link{mclapply}})}
}
\description{
  mcAny checks if a predicate f is true for one or more
  values in x
}
\details{
  NA's obtained while applying f to x will be assumed to be
  FALSE. the user can modify this behaviour by making sure
  the argument f returns TRUE is a value is NA under
  coersion.
}
\keyword{mcAny}

