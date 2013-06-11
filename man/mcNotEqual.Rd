\name{mcNotEqual}
\alias{mcNotEqual}
\title{mcNotEqual}
\usage{
  mcNotEqual(f, g)
}
\arguments{
  \item{f}{a function that returns an object that can be
  compared with ==, or a string giving the name of such a
  function.}

  \item{g}{a function that returns an object that can be
  compared with ==, or a string giving the name of such a
  function.}
}
\value{
  returns a logical value.
}
\description{
  mcNotEqual takes two functions f and g, and returns a
  function. This new function returns f(...) != g(...)
}
\examples{

NULL


}
\keyword{mcNotEqual}

