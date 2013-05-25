\name{mcZipWith}
\alias{mcZipWith}
\title{mcZipWith}
\usage{
  mcZipWith(f, x, paropts = NULL)
}
\arguments{
  \item{f}{a function that takes a single n-element list,
  or a string giving the name of such a function.}

  \item{x}{a list or vector.}

  \item{paropts}{a list of parameters to be handed to
  mclapply (see \link{mchof}).}
}
\value{
  returns the result of mapping f over a list of n element
  lists.
}
\description{
  mcZipWith takes n lists or vectors, generates a list of n
  element lists, and returns the result of mapping f over
  this new list.
}
\details{
  list names are dropped without warning during zipping; an
  example below shows how to add names to the output list.
  NULL elements in x are automatically removed from x. The
  empty list is not removed in order act as a 'zero' to
  preserve useful structural identities.

  the input lists are assumed to be of equal length; if
  they are not excess elements are discarded without
  warning.
}
\examples{

# get the mean of three vectors after zipping

mcZipWith(
	function (x) mean(unlist(x)),
	list(1:4, 2:5, 3:6)
)

# using mcZipWith to add names after zipping

mcZipWith(
	function (x) {
		list(name = x[[1]], id = x[[2]])
	},
	list(
		list('Jane', 'Jill', 'John'),
		list(1, 2, 3)
	),
	list(mc.cores = 2)
)

# or alternatively

mcZipWith(
	function (x) {
		structure(x, names = c('name', 'id'))
	},
	list(
		list('Jane', 'Jill', 'John'),
		list(1, 2, 3)
	),
	list(mc.cores = 2)
)

# add indices to a shuffled vector

mcZipWith (
	function (x) {
		list( x[[1]], ind = x[[2]] )
	}, 
	list(sample(letters[1:10]), 1:10)
)
}
\seealso{
  see \code{\link{mclapply}} for more details about the
  parallel backend being employed, \code{\link{mcZip}} for
  a varient of this function and \code{\link{mcUnzipWith}}
  for the inverse of this function.
}
\keyword{mcZipWith}

