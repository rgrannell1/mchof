#' @title mcPosition
#' 
#' @description Returns the index of the first (or last) position in a vector or list matching 
#' a predicate function, in parallel. 
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

mcPosition <- function (f, x, right=FALSE, paropts=NULL) {
	# multicore version of Position
	
	f <- match.fun(f)

	if (is.null(x) || length(x) == 0) return(integer(0))
	if (is.factor(x)) stop('x may not be a factor')
	
	if (!is.logical(right) || is.na(right)) {
		stop('right must be TRUE or FALSE')
	}
	ncores <- if (!is.null(paropts) && 'mc.cores' %in% names(paropts)) {
		paropts$mc.cores
	} else 1
	
	# this matrix determines which tasks are done in parallel
	
	job_ind <- matrix(NA,
		nrow = ncores,
		ncol = ceiling(length(x)/ncores))
	
	job_ind[seq_along(x)] <- seq_along(x)
	
	job_ind <- if (right && ncores > 1) {
		t(apply(job_ind, 2, rev))
	} else t(job_ind)
	
	job_direction <- if (right) {
		nrow(job_ind):1
	} else 1:nrow(job_ind)
	
	for (i in job_direction) {
		jobs <- job_ind[i,]
		jobs <- jobs[!is.na(jobs)]
		
		checked_ind <- unlist(call_mclapply(
			f = function(j){
				
				is_match <- as.logical(f(x[[j]]))
				if (!is.na(is_match) && is_match) j else NaN	
			},		
			x = jobs, paropts = paropts))
		
		checked_ind <- checked_ind[!is.nan(checked_ind)]
		
		if (length(checked_ind > 0)) {
			return(if (right) max(checked_ind) else min(checked_ind))
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
#' @seealso see \code{\link{Find}} for the non-parallel equivelant of this 
#'     function, \code{\link{mclapply}} for more details about the parallel
#'     backend being employed. 
#'
#' @examples mcFind(
#'     function(x){
#'         grepl('^[J].*$', x)
#'     },
#'     c('mark', 'sam', 'Jane', 'Peter'))
#'

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






