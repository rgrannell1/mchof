#' @description mcPartition returns a list of two lists; a list for which a predicate 
#' returns true, and a list for which a predicate returns false.
#' 
#' @title mcPartition
#' 
#' @export
#' @param f a unary function that returns a boolean value, or a string
#' giving the name of such a function.
#' @param x a list or vector.
#' @param paropts a list of parameters to be handed to 
#'    mclapply (see \link{mchof}).
#'
#' @return returns a list of two lists; the first list contains the values 
#' for which f returned true, the other contains values that returned false or NA. 
#' If the list of true/false elements is empty then the value of that slot is list()
#' if x is a list, and a typed vector such as integer(0) if x is a vector. mcPartition
#' NULL always returns NULL.
#' 
#' @seealso see \code{\link{mcReject}} for a function that 
#' returns the values for which f returns false or NA, and
#' \code{\link{mcFilter}} for a function that returns the values for 
#' which f returns true.
#'
#' @keywords mcPartition
#' @example inst/examples/examples-partition.r

mcPartition <- function (f, x, paropts = NULL) {
	# returns two lists; a list for which f returns 
	# true, and a list for which f returns false
	
	func_call <- paste0( deparse(match.call()), ':' )
	
	missing(f) %throws% stopf (
		'%s a function (or function name) f is required but was missing',
		func_call)
	missing(x) %throws% stopf (
		'%s list/vector x is required but was missing',
		func_call)
	
	f <- match.fun(f)
	if (is.null(x)) return (NULL)
	is.factor(x) %throws% stopf (
		'%s x may not be a factor; actual value was %s (%s)',
		func_call, deparse(x), paste0(class(x), collapse = ', '))
	
	ind <- as.logical(unlist(call_mclapply(f, x, paropts)))
	true_ind <- !is.na(ind) & ind
	
	list (x[true_ind], x[!true_ind])
}
