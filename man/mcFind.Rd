\name{mcFind}
\alias{mcFind}
\title{mcFind}
\usage{
  mcFind(f, x, right = FALSE, paropts = NULL)
}
\arguments{
  \item{f}{a unary function that returns either \code{TRUE}
  or \code{FALSE}}

  \item{x}{a vector}

  \item{right}{a boolean value. Should the first
  \code{TRUE} or last \code{FALSE} element matching
  \code{f} be returned? Defaults to \code{FALSE}}

  \item{paropts}{a list of parameters to be handed to
  mclapply (see details and \code{\link{mclapply}})}
}
\description{
  Returns the value of the first element of x that meets
  the predicate f.
}
\details{
  as with mcFilter, NA's obtained after logical coercion
  are assumed to be FALSE.
}
\examples{
mcFind(
    function(x){
        grepl('^[J].*$', x)
    },
    c('mark', 'sam', 'Jane', 'Peter'))
}
\author{
  Ryan Grannell
}
\seealso{
  see \code{\link{Find}} for the non-parallel equivelant of
  this function, \code{\link{mclapply}} for more details
  about the parallel backend being employed.
}
\keyword{mcFind}

