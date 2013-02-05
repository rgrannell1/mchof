\name{mcFilter}
\alias{mcFilter}
\title{mcFilter}
\usage{
  mcFilter(f, x, paropts = NULL)
}
\arguments{
  \item{f}{a unary function that returns either \code{TRUE}
  or \code{FALSE}}

  \item{x}{a vector}

  \item{paropts}{a list of parameters to be handed to
  mclapply (see details and \code{\link{mclapply}})}
}
\description{
  mcFilter extracts the elements of a vector for which the
  function \code{f} returns true, in parallel
}
\details{
  mcFilter behaves identically to Filter from the user's
  point of view; the results of the function should not
  differ from those obtained using \code{\link{Filter}})
  with similar parameters. However, execution should be
  much faster for particular application than Filter,
  especially if the function \code{f} is
  computation-intensive and a large number of cores are
  available.

  As with \code{\link{Filter}}), NA values obtained during
  filtering are assumed to be \code{FALSE}
}
\examples{
# remove NA values from a vector
p <- function(x) !is.na(x)
mcFilter(p, c(3,2,6,NA, 2))

# the same example, in parallel
p <- function(x) !is.na(x)
mcFilter(p, c(3,2,6,NA, 2, list(mc.cores = 2)))

# a more advanced example, using anonymous functions to
filter out combinations that don't meet a predicate

mcFilter(
    f = function(pair){
        val <- sum(unlist(pair))
 	   if(val > 2 && val < 10) TRUE else FALSE
    },
    x = apply(combn(8, 3), 2, function(x) list(x)) )
}
\seealso{
  see \code{\link{Filter}}) for the non-parallel version of
  this function, \code{\link{mclapply}}) for more details
  about the parallel backend being employed.
}

