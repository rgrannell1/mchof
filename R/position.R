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

## Todo: better search algorithm? still O(n), O(log(n)) may be possible

mcPosition <- function(f, x, right=FALSE, nomatch=NA, paropts=NULL){
	# multicore version of Position
	
	f <- match.fun(f)
	
	if(!is.logical(right) || is.na(right)){
		stop('right must be TRUE or FALSE')
	}
	if(!is.null(paropts) && 'mc.cores' %in% names(paropts)){
		ncores <- paropts$mc.cores
	} else {
		return(Position(f, x, right, nomatch))
	}
	
	ind <- matrix(NA, nrow = ncores,
		ncol = ceiling(length(x)/ncores))
	ind[seq_along(x)] <- seq_along(x)
	ind <- if(right & ncores > 1){
		t(apply(ind, 2, rev))
	} else t(ind)
	
	ind_direction <- if(right){
		seq_len(ncol(ind))
	} else {
		rev(seq_len(ncol(ind)))
	}
	for(i in ind_direction){
		
		res <- Reduce(
			f = rbind, 
			x = call_mclapply(
	   				f = function(j) c(j, f(x[[j]])), 
	   				x = (ind[i,])[!is.na(ind[i,])], paropts) )
			
		res <- res[,1] * res[,2]

		if(any(res[!is.na(res)] != 0)){
			return((ind[i,])[min(which(!is.na(res) & res > 0))])
		}
	}
	nomatch
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
	
	if(!is.logical(right) || is.na(right)){
		stop('right must be TRUE or FALSE')
	}
	if((pos <- mcPosition(f, x, right, nomatch = 0) > 0)){
		x[[pos]]
	}
	else nomatch
}











