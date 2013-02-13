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
	
	to_pairs <- function(flatlist){
		# takes a list of elements x in X, returns a 
		# list of lists, where each lists contains two elements
		# from X, or one in the last nested list if x has odd length
			
		Map(
			function(i){
				
				if(i == length(flatlist)){
					list(first = flatlist[[i]], second = NA)
				} else list(
					first = flatlist[[i]],
					second = flatlist[[i+1]])
			},	
			seq(from = 1, by = 2, len = ceiling(length(flatlist)/2)))
	}

	transitional <- to_pairs(x)
	
	while(length(transitional) > 1){
		
		transitional <- to_pairs(call_mclapply(
			function(trans_vals){
				# returns f(x1, x2), or x1 
				
				if(is.na(trans_vals$second)){
					trans_vals$first
				} else {
					f(trans_vals$first, trans_vals$second)
				}	
			},	
			x = transitional, paropts))
	}
	
	f(transitional[[1]]$first, transitional[[1]]$second) 
}


