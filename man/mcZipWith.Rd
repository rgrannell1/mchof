\name{mcZipWith}
\alias{mcZipWith}
\title{mcZipWith}
\usage{
  mcZipWith(f, ..., paropts = NULL)
}
\arguments{
  \item{f}{a function that takes n arguments, or a string
  giving the name of such a function.}

  \item{...}{several lists or vectors.}

  \item{paropts}{a list of parameters to be handed to
  mclapply (see \link{mchof}).}
}
\value{
  returns the result of mapping f over a list of n element
  lists.
}
\description{
  mcZipWith takes n lists or vectors, generates a list of n
  element lists, and returns the result of calling f with
  each n element list.
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


# a helper function that takes a function that 
# takes on variable and makes it work with ...

squash <- function (f) {
	function (...) f(list(...))
}

# get the mean of three vectors after zipping

# get the list [ [1*1] [2*2] ... [10 * 10] ]

mcZipWith ('*', 1:10, 1:10)

mcZipWith(
	function (...) mean(unlist(...)),
	1:4, 2:5, 3:6
)

# using mcZipWith to add names after zipping

mcZipWith(
	squash(function (x) {
		list(name = x[[1]], id = x[[2]])
	}),
	list('Jane', 'Jill', 'John'),
	list(1, 2, 3),
	paropts = list(mc.cores = 2)
)

# or alternatively

mcZipWith(
	squash(function (x) {
		structure(x, names = c('name', 'id'))
	}),
	list('Jane', 'Jill', 'John'),
	list(1, 2, 3),
	paropts = list(mc.cores = 2)
)

# add indices to a shuffled vector

mcZipWith (
	squash(function (x) {
		list( x[[1]], ind = x[[2]] )
	}), 
	sample(letters[1:10]),
	1:10
)
}
\seealso{
  see \code{\link{mcZip}} for a varient of this function
  and \code{\link{mcUnzipWith}} for the inverse of this
  function.
}
\keyword{mcZipWith}

