\name{mcFilter}
\alias{mcFilter}
\title{mcFilter}
\usage{
  mcFilter(f, x, paropts = NULL)
}
\arguments{
  \item{f}{a unary function that returns a boolean value,
  or a string giving the name of a function.}

  \item{x}{a list or vector. Vectors are converted to lists
  internally.}

  \item{paropts}{paropts a list of parameters to be handed
  to mclapply (see \link{mchof}).}
}
\description{
  mcFilter extracts the elements of a vector or list for
  which the function \code{f} returns \code{TRUE}.
}
\details{
  mcFilter applies f to each element of x, coerces the
  result to a logical value, and returns the values for
  which f returns TRUE. NA's obtained while applying f to x
  will be assumed to be FALSE. the user can sidestep this
  behaviour easily, if necessary (see \link{mchof}).
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
\seealso{
  see \code{\link{mcReject}} for a counterpart to this
  function, and \code{\link{mcPartition}} for a function
  that combines mcFilter and mcReject
}
\keyword{mcFilter}

