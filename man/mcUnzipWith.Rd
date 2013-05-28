\name{mcUnzipWith}
\alias{mcUnzipWith}
\title{mcUnzipWith}
\usage{
  mcUnzipWith(f, x, paropts = NULL)
}
\arguments{
  \item{f}{a function that takes a single n-element list,
  or a string giving the name of such a function.}

  \item{x}{a list of lists or vectors}

  \item{paropts}{paropts a list of parameters to be handed
  to mclapply (see \link{mchof}).}
}
\description{
  mcUnzipWith is the inverse function of mcZipWith; it
  takes a list of n element lists, makes returns n lists
  and applies a function to these lists before returning
  these lists
}
\details{
  list names are dropped without warning during unzipping;
  an example below shows how to add names to the output
  list. NULL elements in x are automatically removed from
  x. The empty list is not removed in order act as a 'zero'
  to preserve useful structural identities.

  the input lists are assumed to be of equal length; if
  they are not excess elements are discarded without
  warning.
}
\examples{

# unzip three lists & convert them to two string vectors

mcUnzipWith(
	'paste0',
	list(
		list('Jane', 1),
		list('Jill', 2),
		list('Joan', 3)
))

# get the mean of each 'column' of lists

mcUnzipWith(
	function (x) mean(unlist(x)),
	list(
		list(0.2, 0.10),
		list(0.5, 0.02),
		list(12.2, 0.2)
))

# name the unzipped output

mcUnzipWith(
	function (x) list(ind_1 = x[1], ind_2 = x[2], ind_3 = x[3]),
	list(
		list('FORTRAN', 'no'),
		list('HASKELL', 'yes'),
		list('CLOJURE', 'yes')
))
}
\seealso{
  see \code{\link{mcZipWith}} for the inverse of this
  function and \code{\link{mcUnzip}} for a shorthand
  variant of this function with f set to identity.
}
\keyword{mcUnzipWith}

