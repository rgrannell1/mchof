#' @description Returns the index of the first (or last) position in a vector or list matching 
#' a predicate function, in parallel. 
#'
#' @title mcPosition
#'  
#' @export
#' @param f a unary function that returns either \code{TRUE} or \code{FALSE}
#' @param x a vector
#' @param right a boolean value. Should the first \code{TRUE} or last 
#'     \code{FALSE} element matching \code{f} be returned? Defaults to \code{FALSE}
#' @param nomatch the variable that is returned if no elements such that
#'     \code{f(element) = TRUE} are found in \code{x}. Defaults to \code{NA} 
#' @param paropts a list of parameters to be handed to 
#'    mclapply (see details and \code{\link{mclapply}})
#'    
#' @details many details
#'    
#' @seealso many links
#'    
#' @examples   
#' mcPosition(function(x) !is.null(x), 10:20, paropts = list(mc.cores = 2))
#' mcPosition(function(x) x > 5, 1:10, right=TRUE, paropts=list(mc.cores = 2))       

mcPosition <- function(f, x, right=FALSE, paropts=NULL){
	# multicore version of Position
	
	f <- match.fun(f)
	
	if(!is.logical(right) || is.na(right)){
		stop('right must be TRUE or FALSE')
	}
	ncores <- if(!is.null(paropts) && 'mc.cores' %in% names(paropts)){
		paropts$mc.cores
	} else 1
	
	# this matrix determines which tasks are done in parallel
	job_ind <- matrix(NA,
		nrow = ncores,
		ncol = ceiling(length(x)/ncores))
	
	job_ind[seq_along(x)] <- seq_along(x)
	
	job_ind <- if(right & ncores > 1){
		t(apply(job_ind, 2, rev))
	} else t(job_ind)
	
	job_direction <- if(right){
		1:ncol(job_ind)
	} else rev(1:nrow(job_ind))
	
	for(i in job_direction){
		parallel_jobs <- na.omit(job_ind[i,])
		
		checked_ind <- unlist(call_mclapply(
			f = function(j){
				# returns the index times a boolean
				
				j * as.logical(f(x[[j]]))
			},		
			x = parallel_jobs, paropts))
	
		if(any(checked_ind != 0)){
			return(min(na.omit(checked_ind) > 0))
		}
	}
	integer(0)
}

mcPosition(function(x) x > 5, 10:1)

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
#' @param nomatch the variable that is returned if no elements such that
#'     \code{f(element) = TRUE} are found in \code{x}. Defaults to \code{NA} 
#' @param paropts a list of parameters to be handed to 
#'    mclapply (see details and \code{\link{mclapply}})
#'

mcFind <- function(f, x, right = FALSE){
	# multicore version of Find

	if((first_match <- mcPosition(f, x, right, nomatch = 0) > 0)){
		x[[first_match]]
	}
	else integer(0)
}











