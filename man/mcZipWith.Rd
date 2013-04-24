\name{mcZipWith}
\alias{mcZipWith}
\title{mcZipWith}
\usage{
  mcZipWith(f, x, paropts = NULL)
}
\arguments{
  \item{f}{a function that takes a single n-element list}

  \item{x}{a list of lists}

  \item{paropts}{a list of parameters to be handed to
  \code{mclapply} (see details)}
}
\description{
  mcZipWith takes n lists/vectors, generates a list of n
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
# adding indices to a list
mcZipWith (
    function (x) {
        list(x[[2]], ind = x[[1]])
    }, list (list(1:10), list(letters[1:10]))
)

# adding names to output
mcZipWith (
    function (x) {
        list(id = x[[1]] , name = x[[2]])
    },
    list ( list('001', '002', '003'),
    list('John', 'Jane', 'Jill')))
}
\author{
  Ryan Grannell
}
\seealso{
  see \code{\link{mclapply}} for more details about the
  parallel backend being employed, \code{\link{mcZip}} for
  a varient of this function and \code{\link{mcUnzipWith}}
  for the inverse of this function.
}
\keyword{mcZipWith}

