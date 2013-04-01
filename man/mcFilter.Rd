\name{mcFilter}
\alias{mcFilter}
\title{mcFilter}
\usage{
  mcFilter(f, x, paropts = NULL)
}
\arguments{
  \item{f}{a unary function that returns a boolean value}

  \item{x}{a list or vector}

  \item{paropts}{a list of parameters to be handed to
  \code{mclapply} (see details)}
}
\description{
  mcFilter extracts the elements of a vector or list for
  which the function \code{f} returns \code{TRUE}, in
  parallel.
}
\details{
  mcFilter applies f to each element of x, coerces the
  result to a logical value, and returns the values for
  which f returns TRUE. NA values obtained during logical
  filtering are assumed to be FALSE, as with \code{Filter}.
  The user can modify this behaviour by making sure the
  argument f returns TRUE is a value is NA under coersion.
}
\examples{
# remove NA values from a vector
p <- function(x) !is.na(x)
mcFilter(p, c(3,2,6,NA, 2))

# the same example, in parallel
p <- function(x) !is.na(x)
mcFilter(p, c(3,2,6,NA, 2, list(mc.cores = 2)))

# find all even numbers in a vector of numbers

even_ints <- function(x){
    Filter(
        f = function(y) if(is.integer(y) && !(y \%\% 2)) TRUE else FALSE,
	       x)
}
even_ints(c(1L,2L,3L,4L,5L,6L,7L,8L,9L,10L))

# a more advanced example, using anonymous functions to
# filter out combinations that don't meet a predicate
mcFilter(
    f = function(pair){
        val <- sum(unlist(pair))
 	   if(val > 8) TRUE else FALSE
    },
    x = apply(combn(8, 3), 2, list),
    paropts = list(mc.cores = 2))
}
\author{
  Ryan Grannell
}
\seealso{
  see \code{\link{Filter}} for a non-parallel equivelant of
  this function, \code{\link{mclapply}} for more details
  about the parallel backend being employed.
}
\keyword{mcFilter}

