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

# create a function to find the sum of a numeric list

list_sum <- sum \%of\% unlist
list_sum( list(1,2,3) )

# compose an anonymous function

capital_number <- toupper \%of\% function (i) letters[i]
capital_number(1)

}
\keyword{mcCompose}

