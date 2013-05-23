
#' @title mcPosition
#' 
#' @description Returns the index of the first (or last) position in a vector or 
#' list matching a predicate function, in parallel.
#'  
#' @export

#' @param f a unary function that returns either \code{TRUE} or \code{FALSE}
#' @param x a list or vector. Vectors are converted to lists internally.
#' @param right a boolean value. Should the list be searched from the start or end first?
#' @param paropts paropts a list of parameters to be handed to 
#'    mclapply (see \link{mchof}).
    
#' @details mcPosition returns integer(0) if no match is found, in much the same
#' way that which(0 == 1) returns integer(0)
#'    
#' @seealso see \code{\link{Position}} for the non-parallel equivelant of this 
#'     function, \code{\link{mclapply}} for more details about the parallel
#'     backend being employed.
#' 
#' @examples
#' # find the index of the first position of the first non-NA value in a vector
#' mcPosition(function(x) !is.na(x), c(10, NA, 11:20), paropts = list(mc.cores = 2))
#' # get the index of the first value larger than five from the rightmost index of a vector
#' mcPosition(function(x) x > 5, 1:10, right=TRUE, paropts=list(mc.cores = 2))       
#' @example inst/examples/examples-position.r
#' @keywords mcPosition

mcPosition <- function (f, x, right=FALSE, paropts=NULL) {
	# returns the first (or last) index in x that matches
	# the predicate f
	
	f <- match.fun(f)

	if (is.null(x) || length(x) == 0) return(integer(0))
	is.factor(x) %throws% stop ('x may not be a factor')
	
	(!is_boolean(right)) %throws% stop (
		'right must be TRUE or FALSE')
	
	ncores <- if (!is.null(paropts) && 'mc.cores' %in% names(paropts)) {
		abs(paropts$mc.cores)
	} else if (!is.null(getOption('mc.cores')))  {
		abs(getOption('mc.cores'))
	} else 1
	
	job_indices <- ihasNext(
		ichunk(
			iterable = if (right) {
				rev(seq_along(x)) 
			} else {
				seq_along(x)
			},
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
#' @param f a unary function that returns either \code{TRUE} or \code{FALSE}
#' @param x a vector
#' @param right a boolean value. Should the first \code{TRUE} or last 
#'     \code{FALSE} element matching \code{f} be returned? Defaults to \code{FALSE}
#' @param paropts a list of parameters to be handed to 
#'    mclapply (see details and \code{\link{mclapply}})
#' 
#' @details as with mcFilter, NA's obtained after logical coercion are assumed to be
#' FALSE.
#' 
#' @seealso see \code{\link{Find}} for the non-parallel equivelant of this 
#'     function, \code{\link{mclapply}} for more details about the parallel
#'     backend being employed. 
#'
#' @examples mcFind(
#'     function(x){
#'         grepl('^[J].*$', x)
#'     },
#'     c('mark', 'sam', 'Jane', 'Peter'))
#' @example inst/examples/examples-find.r
#' @keywords mcFind

mcFind <- function (f, x, right = FALSE, paropts = NULL) {
	# returns the first (or last) element in x that matches
	# the predicate f

	if (is.null(x)) return(NULL)
	if (length(x) == 0) return(x)
	is.factor(x) %throws% stop ('x may not be a factor')
	
	first_match <- mcPosition (f, x, right, paropts)
	
	if (length(first_match) > 0) {
		x[[first_match]]
	}
	else integer(0)
}
