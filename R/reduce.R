#' @title mcReduce
#' 
#' @export
#' @description mcReduce applies an associative binary function to a list,
#' returning a single value
#' 
#' @details \code{mcReduce} can be used as a parallel replacement for a 
#' subclass of problems that can be solved with \code{Reduce} ; parallelising 
#' \code{Reduce} is only possible when the function f is associative; in infix 
#' notation that is \code{(x1 f x2) f x3} is equivalent to \code{x1 f (x2 f x3)}. 
#' In practicality this means that the order in which the reduce is carried out isn't 
#' important, so several tasks can be carried out in parallel.
#'  
#'  A binary function with associativity is the plus operator;
#'  \code{1 + (2 + 3) == (1 + 2) + 3}.
#'  Subtraction does not have this property \code{1 - (2 - 3) != (1 - 2) - 3},
#'  so it should not be used as a binary function for \code{mcReduce} 
#'   
#' @name mcReduce
#' 
#' @param x a vector or list
#' @param paropts paropts a list of parameters to be handed to 
#'    mclapply (see details and \code{\link{mclapply}})
#' @param f a binary function
#'
#' @examples 
#' mcReduce(get('+'), 1:10)
#' mcReduce(rbind, list(c(1, 2), c(3, 4), c(5, 6)))
#' @seealso \code{\link{Reduce}}
#' @keywords mcReduce

mcReduce <- function (f, x, paropts = NULL) {
	# multicore associative f only version of Reduce
	
	to_pairs <- function (flatlist) {
		# takes a list [x1, ..., xn], returns a list of pairs
		# [ [x1, x2], ..., [x(n-1), xn || NULL] ]. If flatlist is odd length 
		# the last element in the last list is NULL.
			
		Map( function(i) {
			
				if (i == length(flatlist)) {
					list(
						first = flatlist[[i]],
						second = NULL)
				} else {
					list(
						first = flatlist[[i]],
						second = flatlist[[i+1]])
				}
			}, seq(from = 1, by = 2, len = ceiling(length(flatlist)/2)))
	}

	if (is.null(x)) return(NULL)
	if (is.list(x) && length(x) == 0) return(list())
	if (length(x) == 1) return(x)
	if (is.factor(x)) stop('x may not be a factor')
	
	f <- match.fun(f)
	reducable <- to_pairs(x)
	
	while (length(reducable) > 1) {
		
		reducable <- to_pairs(
			call_mclapply(
				function (val_pair) {
					
					with (val_pair,	
						if (is.null(second)) {
							first
						} else {
							f(first, second)
						}) 
				},	
				x = reducable, paropts))
	}
	with (reducable[[1]], f(first, second))
}
