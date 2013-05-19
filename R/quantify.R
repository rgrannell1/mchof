
#' @title mcAll
#' 
#' @export
#' @description mcAll checks if a predicate f is true for all values in x
#' 
#' @details NA's obtained while applying f to x will be assumed to be FALSE.
#' the user can modify this behaviour by making sure the argument f returns 
#' TRUE is a value is NA under coersion.
#' 
#' @name mcAll
#' 
#' @param f a unary function that returns a boolean value
#' @param x a list or vector
#' @param paropts paropts a list of parameters to be handed to 
#'    mclapply (see details and \code{\link{mclapply}})
#'    
#' @keywords mcAll
#' 

mcAll <- function (f, x, paropts = NULL) {
	# apply a function f to x, return TRUE iff f is true
	# for all x
	
	f <- match.fun(f)
	if (is.null(x)) return(x)
	if (is.list(x) && length(x) == 0) return(list())
	if (is.factor(x)) stop('x may not be a factor')
	
	bools <- as.logical(call_mclapply(f, x, paropts))
	bools[is.na(bools)] <- FALSE
	
	all(bools)
}

#' @title mcAny
#' 
#' @export
#' @description mcAny checks if a predicate f is true for one or more values in x
#' 
#' @details NA's obtained while applying f to x will be assumed to be FALSE.
#' the user can modify this behaviour by making sure the argument f returns 
#' TRUE is a value is NA under coersion.
#' 
#' @name mcAny
#' 
#' @param f a unary function that returns a boolean value
#' @param x a list or vector
#' @param paropts paropts a list of parameters to be handed to 
#'    mclapply (see details and \code{\link{mclapply}})
#'    
#' @keywords mcAny
#' 

mcAny <- function (f, x, paropts = NULL) {
	# apply a function f to x, return TRUE iff f is true
	# for any x
	
	f <- match.fun(f)
	if (is.null(x)) return(x)
	if (is.list(x) && length(x) == 0) return(list())
	if (is.factor(x)) stop('x may not be a factor')
	
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
#' @description mcOne checks if a predicate f is true for exactly one value in x
#' 
#' @details NA's obtained while applying f to x will be assumed to be FALSE.
#' the user can modify this behaviour by making sure the argument f returns 
#' TRUE is a value is NA under coersion.
#' 
#' @name mcOne
#' 
#' @param f a unary function that returns a boolean value
#' @param x a list or vector
#' @param paropts paropts a list of parameters to be handed to 
#'    mclapply (see details and \code{\link{mclapply}})
#'    
#' @keywords mcOne
#' 

mcOne <- function (f, x, paropts = NULL) {
	# apply a function f to x, return TRUE iff f is true
	# for any x
	
	f <- match.fun(f)
	if (is.null(x)) return(x)
	if (is.list(x) && length(x) == 0) return(list())
	if (is.factor(x)) stop('x may not be a factor')
	
	bools <- as.logical(call_mclapply(f, x, paropts))
	bools[is.na(bools)] <- FALSE
	
	length(which(bools)) == 1
}
