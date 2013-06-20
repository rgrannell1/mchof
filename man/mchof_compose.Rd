\name{mcCompose}
\alias{mcCompose}
\title{Higher-Order-Functions for Composing Functions}
\usage{
  mcCompose(f, g)
}
\description{
  1
}
\examples{

# create a function to find the sum of a numeric list

list_sum <- sum \%of\% unlist
list_sum( list(1,2,3) )

# compose an anonymous function

capital_number <- toupper \%of\% function (i) letters[i]
capital_number(1)

}

