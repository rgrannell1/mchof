#' @title mcReduce
#' 
#' @export
#' @description mcReduce applies an associative binary function to a list,
#' returning a single value
#' 
#' @details mcReduce can be used as a parallel alternative to Reduce if
#' and only if the function f is associative; that is
#' 
#' \code{(a f b) f c == a f (b f c)}, 
#' 
#' where a, b or c are values that f takes. For example, plus is an associative 
#' binary operator, since
#' 
#' \code{(a + b) + c == a + (b + c)}
#' 
#' for any number a, b or c. Minus does not have this property, so it is not 
#' suitable for use with mcFold. Only associative binary functions can be folded 
#' or reduced in parallel. 
#'  
#' When x only has one element it is returned immediately, as there is no way
#' to apply a binary function to a length-one list.
#' 
#' A likely source of errors when using mcFold
#' or mcReduce is using a function without the type signature [A] -> [A] -> [A] (ie. a function that
#' takes two of a thing, and returns one of a thing).
#'   
#' @name mcReduce
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
	
	func_call <- paste0( deparse(match.call()), ':' )

	missing(f) %throws% stopf (
		'%s a function (or function name) f is required but was missing',
		func_call)	
	missing(x) %throws% stopf (
		'%s list/vector x is required but was missing',
		func_call)
	
	f <- match.fun(f)
	if (length(x) < 2) return (x)
	is.factor(x) %throws% stopf (
		'%s x may not be a factor; actual value was %s (%s)',
		func_call, deparse(x), paste0(class(x), collapse = ', '))
	
	g <- function (x) {
		if (length(x) == 2) f( x[[1]], x[[2]] ) else x[[1]]	
	}

	final <- iterateWhile(
		function (reducable) {
			group_into(call_mclapply(g, reducable, paropts), size = 2)
		},
		function (reducable) {
			length(reducable) == 1
		},
		group_into(x, size = 2))
	
	g( final[[1]] )
}
