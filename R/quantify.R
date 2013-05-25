#' @title mcAll
#' 
#' @export
#' @description mcAny checks if a predicate function f is true for all 
#' elements in the list or vector x
#' 
#' @details NA's obtained while applying f to x will be assumed to be FALSE.
#' the user can sidestep this behaviour easily, if necessary (see \link{mchof}). 
#' 
#' @name mcAll
#' 
#' @param f a unary function that returns a boolean value, or a string
#' giving the name of a function.
#' @param x a list or vector.
#' @param paropts paropts a list of parameters to be handed to 
#'    mclapply (see \link{mchof}).
#'    
#' @keywords mcAll
#' @example inst/examples/examples-all.r
#' 
#' @return returns TRUE if f is true for one element in x, 
#' otherwise it returns FALSE. If x is NULL, NULL is returned. If x is an
#' empty list an empty list is returned.

mcAll <- function (f, x, paropts = NULL) {
	# apply a function f to x, return TRUE iff f is true
	# for all x
	
	func_call <- paste0( deparse(match.call()), ':' )

	f <- match.fun(f)
	if (is.null(x)) return(x)
	if (is.list(x) && length(x) == 0) return(list())
	is.factor(x) %throws% stopf ('x may not be a factor', func_call)
	
	bools <- as.logical(call_mclapply(f, x, paropts))
	bools[is.na(bools)] <- FALSE
	
	all(bools)
}

#' @title mcAny
#' 
#' @export
#' @description mcAny checks if a predicate function f is true for one or more 
#' elements in the list or vector x
#' 
#' @details NA's obtained while applying f to x will be assumed to be FALSE.
#' the user can sidestep this behaviour easily, if necessary (see \link{mchof}).
#' 
#' @name mcAny
#' 
#' @param f a unary function that returns a boolean value, or a string
#' giving the name of a function.
#' @param x a list or vector.
#' @param paropts paropts a list of parameters to be handed to 
#'    mclapply (see \link{mchof}).
#'    
#' @keywords mcAny
#' @example inst/examples/examples-any.r
#'
#' @return returns TRUE if f is true for one element in x, 
#' otherwise it returns FALSE. If x is NULL, NULL is returned. If x is an
#' empty list an empty list is returned.
#'

mcAny <- function (f, x, paropts = NULL) {
	# apply a function f to x, return TRUE iff f is true
	# for any x
	
	func_call <- paste0( deparse(match.call()), ':' )

	f <- match.fun(f)
	if (is.null(x)) return(x)
	if (is.list(x) && length(x) == 0) return(list())
	is.factor(x) %throws% stopf ('x may not be a factor', func_call)

	ncores <- if (!is.null(paropts) && 'mc.cores' %in% names(paropts)) {
		abs(paropts$mc.cores)
	} else if (!is.null(getOption('mc.cores')))  {
		abs(getOption('mc.cores'))
	} else 1

	job_indices <- ihasNext(
		ichunk(
			iterable = x,
			chunkSize = ncores
	))
	
	while (hasNext(job_indices)) {
		
		indices_to_check <- unlist(nextElem(job_indices))
		
		checked_ind <- unlist(call_mclapply(
			f = function (ind) {
				# returns indices satisfying f
				
				is_match <- as.logical(f( x[[ind]] ))	
				if (isTRUE(is_match)) ind else NaN
				
			},
			x = indices_to_check,
			paropts
		))
		
		matched_indices <- checked_ind[
			!is.nan(checked_ind) & checked_ind
		]
		
		if (any(matched_indices)) {
			return (TRUE)
		}
	}
	FALSE
}

#' @title mcOne
#' 
#' @export
#' @description mcOne checks if a predicate function f is true for exactly one 
#' element in the list or vector x
#' 
#' @details NA's obtained while applying f to x will be assumed to be FALSE.
#' the user can sidestep this behaviour easily, if necessary (see \link{mchof}).
#' 
#' @name mcOne
#' 
#' @param f a unary function that returns a boolean value, or a string
#' giving the name of a function.
#' @param x a list or vector.
#' @param paropts paropts a list of parameters to be handed to 
#'    mclapply (see \link{mchof}).
#'
#' @return returns TRUE if f is true for one element in x, 
#' otherwise it returns FALSE. If x is NULL, NULL is returned. If x is an
#' empty list an empty list is returned.
#'
#' @example inst/examples/examples-one.r
#' @keywords mcOne
#' 

mcOne <- function (f, x, paropts = NULL) {
	# apply a function f to x, return TRUE iff f is true
	# for one x
	
	func_call <- paste0( deparse(match.call()), ':' )

	f <- match.fun(f)
	
	if (is.null(x)) return(x)
	if (is.list(x) && length(x) == 0) return(list())
	is.factor(x) %throws% stopf ('x may not be a factor', func_call)
	
	bools <- as.logical(call_mclapply(f, x, paropts))
	bools[is.na(bools)] <- FALSE
	
	length(which(bools)) == 1
}
