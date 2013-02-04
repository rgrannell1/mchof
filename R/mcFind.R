#'
#' Returns the value of the first element of x that meets the predicate f.  
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

mcFind <- function(f, x, right = FALSE, nomatch = NULL){
	# multicore version of Find
	
	if((pos <- mcPosition(f, x, right, nomatch = 0L) > 0L)) 
		x[[pos]]
	else nomatch
}