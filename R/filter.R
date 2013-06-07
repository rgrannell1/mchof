#'
#' @title mcFilter
#' @aliases mcSelect
#' 
#' @description mcFilter extracts the elements of a vector or list for 
#' which the function \code{f} returns \code{TRUE}.
#' 
#' @usage mcFilter(f, x, paropts = NULL)
#' 
#' mcSelect(f, x, paropts = NULL)
#' 
#' @export
#' @param f a unary function that returns a boolean value, or a string
#' giving the name of such a function.
#' @param x a list or vector.
#' @param paropts a list of parameters to be handed to 
#'    mclapply (see \link{mchof}).
#'    
#' @details mcFilter applies f to each element of x, coerces the result to a logical value, 
#' and returns the values for which f returns TRUE. NA's obtained while applying f to x will 
#' be assumed to be FALSE. the user can sidestep this behaviour easily, 
#' if necessary (see \link{mchof}).
#' 
#' @seealso see \code{\link{mcReject}} for a counterpart to this function, and
#' \code{\link{mcPartition}} for a function that combines mcFilter and mcReject
#'    
#' @example inst/examples/examples-filter.r 
#' @keywords mcFilter mcSelect

mcFilter <- function (f, x, paropts = NULL) {
	# returns x[i] such that f(x[i]) is true
	
	func_call <- paste0( deparse(match.call()), ':' )

	f <- match.fun(f)
	if (is.null(x)) return(x)
	if (is.list(x) && length(x) == 0) return(list())
	is.factor(x) %throws% stopf (
		'%s x may not be a factor; actual value was %s (%s)',
		func_call, deparse(x), paste0(class(x), collapse = ', '))
	
	ind <- as.logical(unlist(call_mclapply(f, x, paropts)))
	true_ind <- !is.na(ind) & ind
	
	x[true_ind]	
}

#' @export

mcSelect <- mcFilter
