#' @title mcReduce
#' @author Ryan Grannell
#' 
#' @export
#' @description \code{mcReduce} can be used as a parallel replacement for a 
#' subclass of problems that can be solved with \code{Reduce} ; parallelising 
#' \code{Reduce} is only possible when the function f is associative; in infix 
#' notation that is
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
#' @param f a two-argument function
#' @param x a vector or list
#' @param paropts paropts a list of parameters to be handed to 
#'    mclapply (see details and \code{\link{mclapply}})

mcReduce <- function(f, x, paropts = NULL){
	# multicore version of Reduce
	
	to_pairs <- function(flatlist){
		# takes a list [x1, ..., xn], returns a list of pairs
		# [ [x1, x2], ..., [x(n-1), xn | NULL] ]. If flatlist is odd length 
		# the last element in the last list is NULL.
			
		Map(function(i){
				
				if(i == length(flatlist)){
					list(
						first = flatlist[[i]],
						second = NULL)
				} else list(
					first = flatlist[[i]],
					second = flatlist[[i+1]])
			},	
			seq(from = 1, by = 2, len = ceiling(length(flatlist)/2)))
	}

	f <- match.fun(f)
	reducable <- to_pairs(x)
	
	# successively reduce pairs of elements in not_reduce to a single element
	while(length(reducable) > 1){

		reducable <- to_pairs(
			call_mclapply(
				function(val_pair){
					# returns f(x1, x2), or x1 if only one 
					
					with(
						'val_pair',	
						if(is.null(second)) first else f(first, second) ) 
				},	
				x = reducable, paropts))
	}
	
	f(reducable[[1]]$first, reducable[[1]]$second) 
}
