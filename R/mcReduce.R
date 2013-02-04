#' @title  a
#' 
#' @name mcReduce
#' 
#' @param f a binary function
#' @param x a vector or list
#' @param init an \code{R} object of the same kind of the elements of x
#' @param right a boolean value. Should the function be reduced from left to 
#'     right, or from right to left?
#' @param accumulate a boolean value. Should each intermediate step in the Reduce
#'     be returned?
#' @param nomatch the variable that is returned if no elements such that
#'     \code{f(element) = TRUE} are found in \code{x}. Defaults to \code{NA} 
#' @param paropts paropts a list of parameters to be handed to 
#'    mclapply (see details and \code{\link{mclapply}})


is.associative
is.commutive

mcReduce <- function(f, x, init, right, accumulate, is.associative = FALSE,
	paropts = NULL){
	# multicore version of Reduce, iff f is associative
	
	if(!is.associative){
		return(Reduce(f, x, init, right, accumulate))
	}
		
	
	
	
	
	
	
	
}


