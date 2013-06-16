\name{mcIterateWhile}
\alias{mcIterateWhile}
\title{mcIterateWhile}
\usage{
  mcIterateWhile(p, f, x)
}
\arguments{
  \item{p}{a function that takes an element of code{{f(x),
  f(f(x)), f(f(f(x))), ...}} and returns a TRUE/FALSE
  value}

  \item{f}{a function that is repeatedly applied to
  \code{x, f(x), f(f(x)), ...}}

  \item{x}{an initial vector/list to apply f to
  repeatedly.}
}
\description{
  mcIterateWhile
}
\keyword{mcIterateWhile}

