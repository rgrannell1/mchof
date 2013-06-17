\name{mcZipWith}
\alias{mcUnzip}
\alias{mcUnzipWith}
\alias{mcZip}
\alias{mcZipWith}
\title{Higher-Order-Functions for Restructuring Lists/Vectors}
\usage{
  mcZipWith(f, ..., paropts = NULL)

  mcZip(..., paropts = NULL)

  mcUnzipWith(f, x, paropts = NULL)

  mcUnzip(x, paropts = NULL)
}
\arguments{
  \item{f}{a function that takes n arguments, or a string
  giving the name of such a function.}

  \item{...}{several lists or vectors.}

  \item{paropts}{a list of parameters to be handed to
  mclapply (see \link{mchof}).}
}
\description{
  mcZipWith takes n lists or vectors, generates a list of n
  element lists, and returns the result of mapping \code{f}
  over each n element list.

  mcZip takes n lists/vectors, and generates a list of n
  element lists. It is a special case of mcZipWith

  mcUnzipWith is the inverse function of mcZipWith; it
  takes a list of n element lists, generates n lists and
  returns the result of mapping zcode{f} over each
  generated list.

  mcUnzip is the inverse function of mcZip; it takes a list
  of n element lists/vectors, and returns n lists. It is a
  special case of mcUnzipWith
}
\details{
  list names are dropped without warning during zipping and
  unzipping; an example below shows how to add names to the
  output list. NULL elements in x are automatically removed
  from x. The empty list is not removed in order act as a
  'zero' to preserve useful structural identities.

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


# zip two vectors into a list of 2-element lists

mcZip(1:5, letters[1:5])

# zip three lists togerther

mcZip(
	list('R', 'Matlab', 'SAS'),
	list('language', 'language', 'languge'),
	list('GNU', 'not GNU', 'not GNU')
)

# zip two lists, with some elements discarded

mcZip(1:10,letters[1:4])


squash <- function (f) {
	function (...) f(list(...))
}

# unzip three lists & convert them to two string vectors

mcUnzipWith(
	squash(paste0),
	list(
		list('Jane', 1),
		list('Jill', 2),
		list('Joan', 3)
))

# get the mean of each 'column' of lists

mcUnzipWith(
	squash( function (x) mean(unlist(x)) ),
	list(
		list(0.2, 0.10),
		list(0.5, 0.02),
		list(12.2, 0.2)
))

# name the unzipped output

mcUnzipWith(
	squash( function (x) {
		list(ind_1 = x[1], ind_2 = x[2], ind_3 = x[3])
	} ),
	list(
		list('FORTRAN', 'no'),
		list('HASKELL', 'yes'),
		list('CLOJURE', 'yes')
))


# unzip three lists of 2-elements into two 3-element lists

mcUnzip(
	list(
		c('hat', 'dance'),
		c('may', 'pole'),
		c('silicon', 'chip')
))
}
\keyword{mcUnzip,}
\keyword{mcUnzipWith}
\keyword{mcZip,}
\keyword{mcZipWith,}

