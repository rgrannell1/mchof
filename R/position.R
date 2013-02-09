#' @description Returns the index of the first (or last) position in a vector or list matching 
#' a predicate function, in parallel. 
#'
#' @title mcPosition
#'  
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
	
	if(is.null(x) return(x)
	
	if(!is.logical(right) || is.na(right)){
		stop('right must be TRUE or FALSE')
	}
	if(!is.null(paropts) && 'mc.cores' %in% names(paropts)){
		ncores <- paropts$mc.cores
	} else ncores <- 1
	
	ind <- matrix(NA,
		nrow = ncores,
		ncol = ceiling(length(x)/ncores))
	ind[seq_along(x)] <- seq_along(x)
	ind <- if(right & ncores > 1){
		t(apply(ind, 2, rev))
	} else t(ind)
	
	iterate_direction <- if(right){
		seq_len(ncol(ind))
	} else rev(seq_len(ncol(ind)))

	for(i in iterate_direction){
		
		# this whole block of code is awful!
		jobs <- na.omit(ind[i,])
		
		checked_ind <- do.call(
			rbind,	
			call_mclapply(
				f = function(j) j * as.logical(f(x[[j]])),		
				x = jobs, paropts))
		
		if(any(checked_ind != 0)) min(na.omit(checked_ind) > 0)
		
	}
	integer(0)
}

#' @description Returns the value of the first element of x that meets the predicate f.  
#'
#' @title mcFind
#'  
#' @param f a unary function that returns either \code{TRUE} or \code{FALSE}
#' @param x a vector
#' @param right a boolean value. Should the first \code{TRUE} or last 
#'     \code{FALSE} element matching \code{f} be returned? Defaults to \code{FALSE}
#' @param nomatch the variable that is returned if no elements such that
#'     \code{f(element) = TRUE} are found in \code{x}. Defaults to \code{NA} 
#' @param paropts a list of parameters to be handed to 
#'    mclapply (see details and \code{\link{mclapply}})
#'
#' 

mcFind <- function(f, x, right = FALSE, nomatch = NULL){
	# multicore version of Find

	if((pos <- mcPosition(f, x, right, nomatch = 0) > 0)){
		x[[pos]]
	}
	else nomatch
}











