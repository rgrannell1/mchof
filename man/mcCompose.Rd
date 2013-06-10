\name{mcCompose}
\alias{mcCompose}
\alias{\%of\%}
\title{mcCompose}
\usage{
  mcCompose(f, g)

  f %of% g
}
\arguments{
  \item{f}{a function, or a string giving the name of a
  function.}

  \item{g}{a function, or a string giving the name of a
  function.}
}
\value{
  returns a function that returns f(g(...))
}
\description{
  mcCompose takes two functions f and g, and returns a
  function. This new function returns f(g(...))
}
\examples{

}
\keyword{mcCompose}

