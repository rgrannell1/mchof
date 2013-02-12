#' @title mcReduce
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
#' @param is.associative a boolean value. Is f an associative function (i.e)
#' (x1 f x2) f x3 == x1 (f x2 f x3) 
#' @param paropts paropts a list of parameters to be handed to 
#'    mclapply (see details and \code{\link{mclapply}})

mcReduce <- function(f, x, init, paropts = NULL){
	# multicore version of Reduce
	
	to_pairs <- function(x){
		# takes a list of elements x, returns a 
		# 
		
		
	}
	
	job_ind <- seq_len(length(x))
	transitional <- to_pairs(x)

	while(length(transitional) > 1){
		
		transitional <- to_pairs(call_mclapply(
			function(trans_vals){
				# takes two or one argument, returns one
				
				if(length(trans_vals) == 1){
					trans_vals
				} else {
					f(trans_vals[[1]], trans_vals[[2]])
				}	
			},	
			x = transitional, paropts))
	}
	
}






