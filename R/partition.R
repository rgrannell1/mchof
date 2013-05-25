#' @description mcPartition returns a list of two lists; a list for which a predicate 
#' returns true, and a list for which a predicate returns false.
#' 
#' @title mcPartition
#' 
#' @export
#' @param f a unary function that returns a boolean value, or a string
#' giving the name of such a function.
#' @param x a list or vector.
#' @param paropts paropts a list of parameters to be handed to 
#'    mclapply (see \link{mchof}).
#'
#' @return returns a list of two lists; the first list contains the values 
#' for which f returned true, the other contains values that returned false or NA.
#' 
#' @seealso see \code{\link{mcReject}} for a function that returns the values for 
#' which f returns false or NA, and
#' \code{\link{mcFilter}} for a function that returns the values for 
#' which f returns true.
#'
#' @keywords mcPartition
#' @example inst/examples/examples-partition.r

mcPartition <- function (f, x, paropts = NULL) {
	# returns two lists; a list for which f returns 
	# true, and a list for which f returns false
	
	func_call <- paste0( deparse(match.call()), ':' )

	f <- match.fun(f)
	if (is.null(x)) return(x)
	is.factor(x) %throws% stopf (
		'%s x may not be a factor; actual value was %s (%s)',
		func_call, deparse(x), paste0(class(x), collapse = ', '))
	
	ind <- as.logical(call_mclapply(f, x, paropts))

	list (
		x[!is.na(ind) & ind],
		x[is.na(ind) | !ind])
}
