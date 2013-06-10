#'
#' @title mcFold
#' 
#' @export
#' @description mcFold applies an associative binary function to a list,
#' returning a single value. The difference between mcFold & mcReduce is that an
#' initial value can be supplied to mcFold.
#' 
#' @details mcFold can be used as a parallel alternative to Fold if
#' and only if the function f is associative; that is
#'
#' \code{(a f b) f c == a f (b f c)}, 
#' 
#' where a, b or c are values that f takes. For example, plus is an associative 
#' binary operator, since

#' \code{(a + b) + c == a + (b + c)}

#' for any number a, b or c. Minus does not have this property, so it is not 
#' suitable for use with mcFold. Only associative binary functions can be folded 
#' or reduced in parallel. 
#' 
#' Formally the combination of an associative binary operator,
#' an identity element (first) and a set (x) is known as a monoid; the function f
#' has a type signature of [A] -> [A] -> [A]. A likely source of errors when using mcFold
#' or mcReduce is using a function without this type signature (ie. a function that
#' takes two of a thing, and returns one of a thing).
#' 
#' it is often useful to use the identity of f as first, as it can make it 
#' possible to simplify f. For example, lists have an identity element of list()
#' when concatenated, and integers have an identity of 0 under addition. This is 
#' shown below in the example programs given.
#'  
#' @name mcFold
#' 
#' @param f a binary function that takes two of "a thing" and returns one of a "thing".
#' @param first an initial value to be prepended to x
#' @param x a vector or list
#' @param paropts a list of parameters to be handed to 
#'    mclapply (see \link{mchof}).
#'
#' @return returns the result of x1 f x2 f x3 f x4 f ... xn, the value of which
#' is dependent on the function f, and the contents of x. when x is NULL, NULL
#' is automatically returned, as with other mchof functions. when x is a length-zero
#' input such as integer(0) or list() \code{first} is returned.
#' 
#' @example inst/examples/examples-fold.r

mcFold <- function (f, first, x, paropts = NULL) {
	# swaps the commas in first, x1, x2, ..., xn with
	# the function f.
		
	func_call <- paste0( deparse(match.call()), ':' )

	missing(f) %throws% stopf (
		'%s a function (or function name) f is required but was missing',
		func_call)
	missing(first) %throws% stopf (
		'%s initial value first is required but was missing',
		func_call)
	
	if (is.null(x)) return (NULL)
	if (length(x) == 0) return (first)
	
	is.factor(x) %throws% stopf (
		'%s x may not be a factor; actual value was %s (%s)',
		func_call, deparse(x), paste0(class(x), collapse = ', '))
	
	mcReduce(f, x = c(list(first), x), paropts)
	
}
