\name{mcFilter}
\alias{mcFilter}
\alias{mcPartition}
\alias{mcReject}
\alias{mcSelect}
\title{Filter-like Higher-Order-Functions}
\usage{
  mcFilter(f, x, paropts = NULL)

  mcSelect(f, x, paropts = NULL)

  mcReject(f, x, paropts = NULL)

  mcPartition(f, x, paropts = NULL)
}
\description{
  \code{mcFilter} extracts the elements of a vector or list
  \code{x} for which the function \code{f} returns
  \code{TRUE}.

  \code{mcSelect} is an alias for \code{mcFilter}.

  \code{mcReject} extracts the elements of a vector or list
  \code{x} for which the function \code{f} returns
  \code{FALSE}.

  \code{mcPartition} returns a list of two lists/vectors; a
  list for which \code{f} returns \code{TRUE}, and a list
  for which \code{f} returns \code{FALSE}.
}
\details{
  All of these function return \code{NULL} automatically
  when \code{x} is NULL. \code{mcFilter} and
  \code{mcReject} return \code{x} when it is length-zero
  values automatically.

  \code{mcReject} also returns elements for which \code{f}
  returned \code{NA} are included, so that concatenating
  the results of \code{mcFilter} and \code{Reject} will
  give you the original set \code{x} (though probably in
  the wrong order).

  \code{mcPartition} always returns a list of two
  lists/vectors; the first list/vector contains the values
  for which \code{f} returned \code{TRUE}, the other
  contains values that returned \code{FALSE} or \code{NA}.
  When trying to partition a length-zero value a list
  containing two of that value is returned. For example,
  when \code{x} is \code{integer(0)} then

  \code{list( integer(0), integer(0) )}

  is returned.
}
\examples{


NULL
}
\keyword{mcFilter}
\keyword{mcPartition}
\keyword{mcReject}
\keyword{mcSelect}

