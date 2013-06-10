
#' @title mcPosition
#' 
#' @description Returns the index of the first (or last) position in a vector or 
#' list matching a predicate function.
#'  
#' @export

#' @param f a unary function that returns a boolean value, or a string
#' giving the name of such a function.
#' @param x a list or vector.
#' @param right a boolean value. Should the first TRUE or last 
#' FALSE element matching f be returned? Defaults to FALSE.
#' @param paropts a list of parameters to be handed to 
#'    mclapply (see \link{mchof}).
#'
#' @return returns an integer. If no match is found or x is length(0)
#' integer(0) is returned. This is consistent with the behaviour of which(). As with
#' other mchof function x = NULL returns NULL
#' @details mcPosition can be used as a functional alternative to \code{which}, that works well in 
#' combination with other functionals in base R and this library.
#' NA's obtained while applying f to x will 
#' be assumed to be FALSE. the user can sidestep this behaviour easily, 
#' if necessary (see \link{mchof}).
#'
#' @seealso see \link{which} for an alternate indexing function, and \link{mcFind} for
#' a function which returns the first (or last) element in a list matching a predicate.
#'   
#' @example inst/examples/examples-position.r
#' @keywords mcPosition
#' 

mcPosition <- function (f, x, right=FALSE, paropts=NULL) {
	# returns the first (or last) index in x that matches
	# the predicate f
		
	func_call <- paste0( deparse(match.call()), ':' )
	
	f <- match.fun(f)

	missing(f) %throws% stopf (
		'%s a function (or function name) f is required but was missing',
		func_call)
	missing(x) %throws% stopf (
		'%s list/vector x is required but was missing',
		func_call)
	
	if (is.null(x)) return (NULL)
	if (length(x) == 0) return (integer(0))
	is.factor(x) %throws% stopf (
		'%s x may not be a factor; actual value was %s (%s)',
		func_call, deparse(x), paste0(class(x), collapse = ', '))
	
	(!is_boolean(right)) %throws% stopf (
		'right must be TRUE or FALSE', func_call)
	
	cores <- get_cores(paropts)

	job_indices <- if (right) {
		rev(group_into(seq_along(x), cores))
	} else {
		group_into(seq_along(x), cores) 
	}

	for (i in seq_along(job_indices)) {
		
		checked_ind <- unlist(call_mclapply(
			f = function (ind) {
				# returns indices satisfying f
				
				is_match <- as.logical(f( x[[ind]] ))	
				if (isTRUE(is_match)) ind else NaN
			},
			x = job_indices[[i]], paropts
		))
		
		matched_indices <- checked_ind[
			!is.nan(checked_ind) & checked_ind
		]
		
		if (length(matched_indices) > 0) {
			return (if (right) max(matched_indices) else min(matched_indices))
		}
	}
	integer(0)
}

#' @description Returns the value of the first element of x that meets the predicate f.  
#'
#' @title mcFind
#' 
#' @export
#' @param f a unary function that returns a boolean value, or a string
#' giving the name of such a function.
#' @param x a list or vector.
#' @param right a boolean value. Should the first TRUE or last 
#' FALSE element matching f be returned? Defaults to FALSE.
#' @param paropts a list of parameters to be handed to 
#'    mclapply (see \link{mchof}).
#' 
#' @return returns an element from x. If x is NULL, NULL is returned. If
#' x is the empty list, the empty list is returned.
#' 
#' @details mcFind applies f to each element of x, coerces the result 
#' to a logical value, and returns the first value for which f returns TRUE. 
#' NA's obtained while applying f to x will 
#' be assumed to be FALSE. the user can sidestep this behaviour easily, 
#' if necessary (see \link{mchof}).
#' 
#' @seealso see \code{mcPosition} to return the index of the first element
#' matching f.
#'
#' @example inst/examples/examples-find.r
#' @keywords mcFind

mcFind <- function (f, x, right = FALSE, paropts = NULL) {
	# returns the first (or last) element in x that matches
	# the predicate f

	func_call <- paste0( deparse(match.call()), ':' )

	if (is.null(x)) return (NULL)
	if (length(x) == 0) return (x)
	is.factor(x) %throws% stopf (
		'%s x may not be a factor; actual value was %s (%s)',
		func_call, deparse(x), paste0(class(x), collapse = ', '))
	
	first_match <- mcPosition (f, x, right, paropts)
	
	if (length(first_match) > 0) {
		x[[first_match]]
	}
	else integer(0)
}
