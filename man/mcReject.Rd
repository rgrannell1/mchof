\name{mcReject}
\alias{mcReject}
\title{mcReject}
\usage{
  mcReject(f, x, paropts = NULL)
}
\arguments{
  \item{f}{a unary function that returns a boolean value,
  or a string giving the name of a function.}

  \item{x}{a list or vector. Vectors are converted to lists
  internally.}

  \item{paropts}{paropts a list of parameters to be handed
  to mclapply (see \link{mchof}).}
}
\value{
  returns a list of elements for which f returned FALSE or
  NA.
}
\description{
  mcReject extracts the elements of a vector or list for
  which the function \code{f} returns \code{FALSE}.
}
\details{
  mcReject applies f to each element of x, coerces the
  result to a logical value, and returns the values for
  which f returns FALSE. elements for which f returned NA
  are included, mcReject will give you the original set x
  (though unordered). The user can modify this behaviour by
  making sure the argument f returns TRUE is a value is NA
  under coersion, as described in \link{mchof}.
}
\seealso{
  see \code{mcFilter} for a complementary function to this,
  and \code{mcPartition} for a function that combines
  mcFilter and mcReject
}

