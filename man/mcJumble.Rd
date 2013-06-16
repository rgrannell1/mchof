\name{mcJumble}
\alias{mcJumble}
\title{mcJumble}
\usage{
  mcJumble(f, x)
}
\arguments{
  \item{f}{a function with normal parameters and/or
  ellipses (...).}

  \item{x}{a vector that is the same length as the formal
  parameters of x and contains all the integers 1, 2, ...,
  length(formals). This vector determines how the formals
  of f will be permuted (see details).}
}
\value{
  returns f with its formal parameters permuted by x.
}
\description{
  mcJumble takes a function f, and returns f with its
  formal arguments reversed.
}
\details{
  mcJumble can operate on functions with ellipses (...) and
  normal parameters, but it cannot currently operate on
  functions with primitive functions such as "+". mcJumble
  preserves the default arguments of f.

  the vector x determines how the formals of f will be
  permuted. For example, if \code{x = c(3, 1, 2), f =
  function (a, b, c) sum(a, b, c)} the new formals are c,
  a, b; the 3rd element in formals, the 1st element in
  formals and the 2nd element in formals. The vector x
  defines a mapping between
}
\examples{

NULL
}

