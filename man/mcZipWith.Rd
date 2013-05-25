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
  Names are dropped without warning during zipping; named
  outputs are given in the example below. mcZipWith
  discards excess elements without warning: for example
  \code{list (1, 2), list (3, 4, 5)} becomes \code{list (f(
  list(1, 3) ), f( list(2, 4) ))}.
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

