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
#' mcReduce is useful when used with mathematical operators such as plus, or
#' max; mcFold is more apt at accumulating data since the first option simplifies the
#' f function. Both mcReduce and mcFold may be used to emulate tail recursion over a list.
#'   
#' @name mcReduce
#' 
#' @param f a binary function
#' @param x a list or vector.
#' @param paropts paropts a list of parameters to be handed to 
#'    mclapply (see the vignette).
#'    
#' @return returns the result of x1 f x2 f x3 f x4 f ... xn, the value of which
#' is dependent on the function f, and the contents of x.
#'
#' @example inst/examples/examples-reduce.r
#' @seealso \code{\link{Reduce}}
#' @keywords mcReduce

to_pairs <- function (x) {
	# chunk x into lists of two, where possible
	
	group_into(x, 2)
}

iterateWhile <- function (f, p, x) {
	# pipe the output x of f into f, 
	# until p(x) is true
	
	while( !p(x) ) x <- f(x)
	x
} 

mcReduce <- function (f, x, paropts = NULL) {
	# swaps the commas in x1, x2, x3, ..., xn with
	# the infix function f.
	
	func_call <- paste0( deparse(match.call()), ':' )
	
	f <- match.fun(f)

	if (is.null(x)) return(NULL)
	if (is.list(x) && length(x) == 0) return(list())
	if (length(x) == 1) return(x)
	is.factor(x) %throws% stopf (
		'%s x may not be a factor; actual value was %s (%s)',
		func_call, deparse(x), paste0(class(x), collapse = ', '))
	
	g <- function (x) {
		if (length(x) == 2) f( x[[1]], x[[2]] ) else x[[1]]	
	}

	g( iterateWhile (
		function (reducable) {
			to_pairs(call_mclapply(g, reducable, paropts))
		},
		function (reducable) {
			length(reducable) == 1
		},
		to_pairs(x)) [[1]] )

}
