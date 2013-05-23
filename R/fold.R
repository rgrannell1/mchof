#' @title mcFold
#' 
#' @export
#' @description mcFold applies an associative binary function to a list,
#' returning a single value. The difference between mcFold & mcReduce is that an
#' initial value can be supplied to mcFold, making certain tasks easier (see examples).
#' 
#' @details mcFold can be used as a parallel alternative to Fold if
#' and only if the function f is associative; that is
#'
#' \code{(a f b) f c == a f (b f c)}, 
#' 
#' where a, b or c are values that f takes. For example, plus is an associative 
#' binary operator, since

#' \code{(a + b) + c == a + (b + c)}

#' for any number a, b or c. Minus does not have this property, so it is not 
#' suitable for use with mcFold. Only associative binary functions can be folded 
#' or reduced in parallel.
#' 
#' it is often useful to use the identity of f as first, as it can make it 
#' possible to simplify f. For example, lists have an identity element of list()
#' when concatenated, and integers have an identity of 0 under addition. This is 
#' shown below in the example programs given 
#' 
#' @name mcFold
#' 
#' @param f a binary function
#' @param first an initial value for the first position of f
#' @param x a vector or list
#' @param paropts paropts a list of parameters to be handed to 
#'    mclapply (see details and \code{\link{mclapply}})
#'
#' @return returns the result of x1 f x2 f x3 f x4 f ... xn, the value of which
#' is dependent on the function f, and the contents of x.


mcFold <- function (f, first, x, paropts = NULL) {
	# swaps the commas in first, x1, x2, ..., xn with
	# the function f.
	
	mcReduce(f, c(first,x), paropts)
	
}
