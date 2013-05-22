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
#' A binary function with associativity is the plus operator;
#' \code{1 + (2 + 3) == (1 + 2) + 3}.
#' Subtraction does not have this property \code{1 - (2 - 3) != (1 - 2) - 3},
#' so it should not be used as a binary function for \code{mcReduce} 
#'  
#' When x only has one element it is returned immediately, as there is no way
#' to apply a binary function to a length-one list.
#'   
#' @name mcReduce
#' 
#' @param f a binary function
#' @param x a vector or list
#' @param paropts paropts a list of parameters to be handed to 
#'    mclapply (see details and \code{\link{mclapply}})
#'
#' @examples 
#' mcReduce(get('+'), 1:10)
#' mcReduce(rbind, list(c(1, 2), c(3, 4), c(5, 6)))
#' @seealso \code{\link{Reduce}}
#' @keywords mcReduce

mcReduce <- function (f, x, paropts = NULL) {
	# multicore associative-only version of Reduce

	if (is.null(x)) return(NULL)
	if (is.list(x) && length(x) == 0) return(list())
	if (length(x) == 1) return(x)
	is.factor(x) %throws% stop ('x may not be a factor')
	
	f <- match.fun(f)

	to_pairs <- function (x) {
		# chunk x into lists of two, where possible
		
		as.list(ichunk(x, 2))
	}
	pair_fmap <- function (f) {
		# returns a function that applies f to pairs:
		# g(a, b) |-> f(a, b), g(a) |-> a
		
		function (x) {
			if (length(x) == 2) f(x[[1]], x[[2]]) else x	
		}	
	}
	iterateWhile <- function (f, p, x) {
		# pipe the output x of f into f, 
		# until p(x) is true
		
		while( !p(x) ) x <- f(x)
		x
	} 
	
	iterateWhile (
		function (reducable) {
			to_pairs(call_mclapply(pair_fmap(f), reducable, paropts))
		},
		function (reducable) {
			length(reducable) == 1
		},
		to_pairs(x)
	)
}
