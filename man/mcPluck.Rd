\name{mcPluck}
\alias{mcPluck}
\title{mcPluck}
\usage{
  mcPluck(pattern, x, paropts = NULL)
}
\arguments{
  \item{name}{a name or regular expression for keys to be
  returned.}

  \item{x}{a list or vector.}

  \item{paropts}{a list of parameters to be handed to
  mclapply (see \link{mchof}).}
}
\description{
  mcPluck iterates over a list x, and extract slots
  matching name in each sublist. If x is a vector then keys
  matching name are returned.
}
\details{
  mcPluck can operate on lists in which not every sublist
  contains a named slot matching pattern. If multiple
  matches are found in a sublist they are all returned.
}
\examples{

NULL
}
\keyword{mcPluck}

