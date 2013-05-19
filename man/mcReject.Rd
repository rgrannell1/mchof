\name{mcReject}
\alias{mcReject}
\title{mcReject}
\usage{
  mcReject(f, x, paropts = NULL)
}
\arguments{
  \item{f}{a unary function that returns a boolean value}

  \item{x}{a list or vector}

  \item{paropts}{a list of parameters to be handed to
  \code{mclapply} (see details)}
}
\description{
  mcReject extracts the elements of a vector or list for
  which the function \code{f} returns \code{FALSE}.
}
\details{
  mcReject applies f to each element of x, coerces the
  result to a logical value, and returns the values for
  which f returns FALSE. NA values obtained during logical
  filtering are assumed to be FALSE, so that concatenating
  the results of mcFilter and mcReject will give you the
  original set x (though unordered). The user can modify
  this behaviour by making sure the argument f returns TRUE
  is a value is NA under coersion.
}
\seealso{
  see \code{mcFilter} for a complementary function to this,
  and \code{mcPartition} for a function that combines
  mcFilter and mcReject
}

