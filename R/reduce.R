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
		# takes a list of elements x in X, returns a 
		# list of lists, where each lists contains two elements
		# from X, or one in the last nested list if x has odd length
		
		pairs <- list()
		
		while(length(x) > 0){
			
			if(length(x) == 1){
				to_push <- list(first = x[[1]], second = NA)
				x <- tail(x, -1)
			} else{
				to_push <- list(first = x[[1]], second = x[[2]])
				x <- tail(x, -2)
			}
			pairs <- c(pairs, list(to_push))
		}
		return(pairs)
	}

	transitional <- to_pairs(x)
	
	while(length(transitional) > 1){
	
		transitional <- to_pairs(call_mclapply(
			function(trans_vals){
				# takes two or one argument, returns one

				if(is.na(trans_vals$second)){
					trans_vals$first
				} else {
					f(trans_vals$first, trans_vals$second)
				}	
			},	
			x = transitional, paropts))
	}
	
	collapsed <- f(transitional[[1]]$first, transitional[[1]]$second) 
	return(collapsed)
}

mcReduce(f = get('+'), 1:10, 1)

