\name{mcFlip}
\alias{mcFlip}
\title{mcFlip}
\usage{
  mcFlip(f)
}
\arguments{
  \item{f}{a function with normal parameters and/or
  ellipses (...).}
}
\value{
  returns f with its formal parameters reversed.
}
\description{
  mcFlip takes a function f, and returns f with its formal
  arguments reversed.
}
\details{
  mcFlip can operate on functions with ellipses (...) and
  normal parameters, but it cannot currently operate on
  functions with primitive functions such as "+". mcFlip
  preserves the default arguments of f.
}
\examples{

NULL
}

