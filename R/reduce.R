#' @title mcReduce
#' @author Ryan Grannell
#'  
#' @description \code{mcReduce} can be used as a parallel replacement for a 
#' subclass of problems that can be solved with \code{Reduce} ; parallelising 
#' \code{Reduce}  is only possible when the function f is associative; that is
#' 
#' \code{(x1 f x2) f x3} is equivalent to \code{x1 f (x2 f x3)}. In practicality
#' this means that the order in which the Reduce is carried out isn't important,
#' so several tasks can be carried out in parallel.
#'  
#'  A binary function with associativity is the plus operator;
#'  \code{1 + (2 + 3) == (1 + 2) + 3}.
#'  Subtraction does not have this property \code{1 - (2 - 3) != (1 - 2) - 3},
#'  so it should not be used as a binary function for \code{mcReduce} 
#'   
#' @name mcReduce
#' 
#' @param f a binary function
#' @param x a vector or list
#' @param paropts paropts a list of parameters to be handed to 
#'    mclapply (see details and \code{\link{mclapply}})

mcReduce <- function(f, x, paropts = NULL){
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

	f <- match.fun(f)
	
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


