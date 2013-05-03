#' @title mcPosition
#' 
#' @description Returns the index of the first (or last) position in a vector or 
#' list matching a predicate function, in parallel. 
#'  
#' @export
#' @param f a unary function that returns either \code{TRUE} or \code{FALSE}
#' @param x a vector or list
#' @param right a boolean value. Should the first \code{TRUE} or last 
#'     \code{FALSE} element matching \code{f} be returned? Defaults to \code{FALSE}
#' @param paropts a list of parameters to be handed to 
#'    mclapply (see details and \code{\link{mclapply}})
#'    
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
#' @keywords mcPosition

mcPosition <- function (f, x, right=FALSE, paropts=NULL) {
	# multicore version of Position
	
	f <- match.fun(f)

	if (is.null(x) || length(x) == 0) return(integer(0))
	if (is.factor(x)) stop('x may not be a factor')
	
	if (!is.logical(right) || is.na(right)) {
		stop('right must be TRUE or FALSE')
	}
	ncores <- if (!is.null(paropts) && 'mc.cores' %in% names(paropts)) {
		abs(paropts$mc.cores)
	} else if (!is.null(getOption('mc.cores')))  {
		abs(getOption('mc.cores'))
	} else 1
	
	steps_needed <- ceiling(length(x)/ncores)
	
	job_ind <- function (i) {
	
		stopifnot(1 + ((i-1) * ncores) <= length(x))
		seq(
	 		from = 1 + ((i-1) * ncores),
	 		to = min((i * ncores), length(x)))
	}
	
	job_direction <- if (right) {
		steps_needed:1
	} else 1:steps_needed
	
	for (i in job_direction) {

		jobs <- job_ind(i)
		
		checked_ind <- unlist(call_mclapply(
			f = function(j){
				
				is_match <- as.logical(f(x[[j]]))
				if (!is.na(is_match) && is_match) j else NaN	
			},		
			x = jobs, paropts = paropts))
		
		matched_ind <- checked_ind[!is.nan(checked_ind)]
		
		if (length(matched_ind) > 0) {
			return(if (right) max(matched_ind) else min(matched_ind))
		}
	}
	integer(0)
}

#' @description Returns the value of the first element of x that meets the predicate f.  
#'
#' @author Ryan Grannell
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
#' @keywords mcFind

mcFind <- function (f, x, right = FALSE, paropts = NULL) {
	# multicore version of Find

	if (is.null(x)) return(NULL)
	if (length(x) == 0) return(x)
	if (is.factor(x)) stop('x may not be a factor')
	
	first_match <- mcPosition (f, x, right, paropts)
	
	if (length(first_match) > 0) {
		x[[first_match]]
	}
	else integer(0)
}
