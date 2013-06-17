
#' Functions for Modifying Function Parameters
#'
#' @description
#'
#'\code{mcFlip} mcFlip takes a function \code{f}, and returns \code{f} with its parameters
#' reversed.
#'
#'\code{mcJumble} takes a function \code{f}, and returns \code{f} with its parameters
#' permuted as defined by \code{x}.
#
#'
#' @details 
#'
#' the vector x determines how the formals of f will be permuted. For example,
#' if \code{x = c(3, 1, 2), f = function (a, b, c) sum(a, b, c)} the new formals 
#' are c, a, b; the 3rd element in formals, the 1st element in formals and the 
#' 2nd element in formals.
#'
#' @param f a function, or a string giving the name of a function.
#' @param x a vector that is the same length as the formal parameters of x and contains
#' all the integers 1, 2, ..., length(arguments). This vector determines how the 
#' arguments of f will be permuted.

#' @rdname mchof_parameters
#' @family mchof-parameters
#' @example inst/examples/examples-parameters.r
#' @export

mcFlip <- function (f) {
	# return a function with reversed formal arguments
	
	func_call <- "mcFlip(f)"

	missing(f) %throws% messages$function_is_required(func_call, "f")

	f <- match.fun(f)
	if (length(formals(f)) < 2) return (f)

	ISSUE("flip need to support primitives")

	formals(f) <- rev(formals(f))
	f
}

#' @rdname mchof_parameters
#' @family mchof-parameters
#' @export

mcJumble <- function (f, x) {
	# returns a function with its formals rearranged, 
	# according to a index vector (bijection) 
	
	func_call <- "mcJumble(f, x)"
	
	missing(f) %throws% messages$function_is_required(func_call, "f")
	missing(x) %throws% messages$vector_is_required(func_call, "x")

	is.factor(x) %throws% messages$was_a_factor(func_call, x, "x")
	
	f <- match.fun(f)
	if (length(formals(f)) < 2) return (f)
	
	( !all(x %in% seq_along(formals(f))) ) %throws% 
		messages$must_be_indices(func_call, x, "x")
	
	(length(formals(f)) != length(x)) %throws% 
		messages$length_mismatch(
			func_call, c(length(formals(f)),
			length(x)), "formals(f)", "x")

	(any_duplicated(x)) %throws% 
		messages$matched_multiple_time(func_call, x, "x")
		
	ISSUE("jumble need to support primitives")

	formals(f) <- formals(f)[c(x)]
	f
}


mcExplode <- function (f) {
	# takes a function that takes a single value and 
	# makes it into a variadic function

	function (...) {
		do.call(f, list(...))
	}
}

mcImplode <- function (f) {
	# takes a function that takes a many values and 
	# makes it into a function that takes one list

	function (x) {
		do.call(f, c(list(), x))
	}
}





