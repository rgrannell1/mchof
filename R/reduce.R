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

	not_reduced <- to_pairs(x)
	
	while(length(not_reduced) > 1){
		
		not_reduced <- to_pairs(call_mclapply(
			function(val_pair){
				# returns f(x1, x2), or x1 
				
				if(is.na(val_pair$second)){
					val_pair$first
				} else {
					f(val_pair$first, val_pair$second)
				}	
			},	
			x = not_reduced, paropts))
	}
	
	f(not_reduced[[1]]$first, not_reduced[[1]]$second) 
}


