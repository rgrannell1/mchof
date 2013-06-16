#'
#' @title mcFold
#' @name mcFold
#' @export
#'
#' @description mcFold applies an associative binary function to a list,
#' returning a single value. The difference between mcFold & mcReduce is that an
#' initial value can be supplied to mcFold.
#' 
#' @template folds
#'
#' @param first an initial value to be prepended to x
#'
#' @return returns the result of x1 f x2 f x3 f x4 f ... xn, the value of which
#' is dependent on the function f, and the contents of x. when x is NULL, NULL
#' is automatically returned, as with other mchof functions. when x is a length-zero
#' input such as integer(0) or list() \code{first} is returned.
#' 
#' @section Special Cases:
#'
#' when x is NULL, NULL is automatically returned (since NULL falls throught all mchof functions without 
#' being interperated as meaningful data). If x is a length-zero value such as list() or integer(0) then
#' first is automatically returned.
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

#' @title mcReduce
#' 
#' @export
#'   
#' @name mcReduce
#' 
#' @template folds
#'    
#' @return returns the result of x1 f x2 f x3 f x4 f ... xn, the value of which
#' is dependent on the function f, and the contents of x. Returns a length-zero or
#' length-one value of x as is.
#'
#' @section Special Cases:
#'
#' if x is a length-zero value such as NULL or integer(0), or a length-one value then x is automatically returned.
#' Length-one values are returned because a binary function cannot be applied to a single value, so the value is 
#' presumed to be already fully "reduced".
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
