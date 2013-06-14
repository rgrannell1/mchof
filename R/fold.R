#'
#' @title mcFold
#' 
#' @export
#' @description mcFold applies an associative binary function to a list,
#' returning a single value. The difference between mcFold & mcReduce is that an
#' initial value can be supplied to mcFold.
#'  
#' @name mcFold
#' 
#' @template folds
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
		
	func_call <- "mcFold(f, first, x, paropts = NULL)"

	missing(f) %throws% messages$function_is_required(func_call, "f")
	missing(first) %throws% messages$vector_is_required(func_call, "first")

	if (is.null(x)) return (NULL)
	if (length(x) == 0) return (first)
	
	is.factor(x) %throws% messages$was_factor(func_call, x, "x")
	
	mcReduce(f, x = c(list(first), x), paropts)
	
}
