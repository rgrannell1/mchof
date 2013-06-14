#' @title mcReduce
#' 
#' @export
#'   
#' @name mcReduce
#' 
#' @template folds
#' 
#' @param f a binary function that takes two of "a thing" and returns one of a "thing".
#' @param x a list or vector.
#' @param paropts a list of parameters to be handed to 
#'    mclapply (see \link{mchof}).
#'    
#' @return returns the result of x1 f x2 f x3 f x4 f ... xn, the value of which
#' is dependent on the function f, and the contents of x. Returns a length-zero or
#' length-one value of x as is.
#'
#' @example inst/examples/examples-reduce.r
#' @seealso \code{\link{Reduce}}
#' @keywords mcReduce

mcReduce <- function (f, x, paropts = NULL) {
	# swaps the commas in x1, x2, x3, ..., xn with
	# the infix function f.

	iterateWhile <- function (f, p, x) {
		# pipe the output x of f into f, 
		# until p(x) is true
		
		while( !p(x) ) x <- f(x)
		x
	} 
	
	func_call <- "mcReduce(f, x, paropts = NULL)"

	missing(f) %throws% messages$function_is_required(func_call, "f")
	missing(x) %throws% messages$vector_is_required(func_call, "x")
		
	f <- match.fun(f)
	if (length(x) < 2) return (x)
	is.factor(x) %throws% messages$was_factor(func_call, x, "x")
	
	g <- function (x) {
		if (length(x) == 2) f( x[[1]], x[[2]] ) else x[[1]]	
	}

	final <- iterateWhile(
		function (reducable) {
			group_into(call_mclapply(g, reducable, paropts, func_call), size = 2)
		},
		function (reducable) {
			length(reducable) == 1
		},
		group_into(x, size = 2))
	
	g( final[[1]] )
}
