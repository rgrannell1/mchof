
#' @title mcReject

#' @description mcReject extracts the elements of a vector or list for  which the function 
#' \code{f} returns \code{FALSE}.
#' 
#' @export
#' @param f a unary function that returns a boolean value, or a string
#' giving the name of such a function.
#' @param x a list or vector.
#' @param paropts a list of parameters to be handed to 
#'    mclapply (see \link{mchof}).

#' @return returns a list of elements for which f returned FALSE or NA.

#' @details mcReject applies f to each element of x, coerces the result to a logical value,
#' and returns the values for which f returns FALSE.
#' 
#' mcReject is more useful for filtering out NULL or NA values in a list that mcFilter,
#' as is demonstrated in the examples below.
#' 
#' elements for which f returned NA are included, so that concatenating the results of mcFilter and 
#' mcReject will give you the original set x (though unordered). The user can
#' modify this behaviour by making sure the argument f returns TRUE is a value 
#' is NA under coersion, as described in \link{mchof}.

#' @seealso see \code{mcFilter} for a complementary function to this, and 
#' \code{mcPartition} for a function that combines mcFilter and mcReject
#'
#' @example inst/examples/examples-reject.r

mcReject <- function (f, x, paropts = NULL) {
	# returns x[i] such that f(x[i]) is false
	
	func_call <- deparse(match.call())

	missing(f) %throws% stopf (
		'%s a function (or function name) f is required but was missing',
		func_call)
	missing(x) %throws% stopf (
		'%s list/vector x is required but was missing',
		func_call)
	
	f <- match.fun(f)
	g <- function (...) {
		res <- as.logical(f(...))
		!isTRUE(res)
	}

	if (is.null(x)) return (NULL)
	if (length(x) == 0) return (x)
	is.factor(x) %throws% stopf (
		'%s x may not be a factor; actual value was %s (%s)',
		func_call, deparse(x), paste0(class(x), collapse = ', '))
	
	ind <- as.logical(unlist(call_mclapply(f, x, paropts)))
	true_ind <- !is.na(ind) & ind
	
	x[!true_ind]	
}
