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
  returns a composed function that inputs arguments,
  applies g to these arguments and then applies f to the
  result of that computation.
}
\description{
  mcCompose takes two functions f and g, and returns a
  function. This new function returns f(g(...))
}
\details{
  This function tries to preserve the formals arguments of
  its input functions in the composite output function.
  There are ways three ways in which the composite function
  might be outputted:

  1, If the function \code{f} and \code{g} have the same
  parameter names and the same default arguments - in the
  same order - then the output function will preserve the
  parameters and default arguments of \code{f}/\code{g}.

  2, If the function \code{f} and \code{g} have the same
  parameter names - in the same order - but have different
  default arguments, then the output function will preserve
  only the parameters of \code{f}/\code{g}.

  3, If the parameters of \code{f} and \code{g} differ, or
  their order is shuffled then the output function uses
  ellipses as parameters.

  primitive functions (such as \code{max}) are supported by
  mchof, and their arguments are processed in a similar way
  to those of closures (standard R functions).
}
\examples{

# create a function to find the sum of a numeric list

list_sum <- sum \%of\% unlist
list_sum( list(1,2,3) )

# compose an anonymous function

capital_number <- toupper \%of\% function (i) letters[i]
capital_number(1)

}
\keyword{mcCompose}

