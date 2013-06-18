
#' Functions for Modifying Function Parameters
#'
#' @description
#'
#'\code{mcFlip} mcFlip takes a function \code{f}, and returns \code{f} with its parameters
#' reversed.
#'
#'\code{mcJumble} takes a function \code{f}, and returns \code{f} with its parameters
#' permuted as defined by \code{x}.
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
	if (length(parametres(f)) < 2) return (f)

	parametres(f) <- rev(formals(f))
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
	if (length(parametres(f)) < 2) return (f)
	
	( !all(x %in% seq_along(parametres(f))) ) %throws% 
		messages$must_be_indices(func_call, x, "x")
	
	(length(parametres(f)) != length(x)) %throws% 
		messages$length_mismatch(
			func_call, c(length(parametres(f)),
			length(x)), "parametres(f)", "x")

	(any_duplicated(x)) %throws% 
		messages$matched_multiple_time(func_call, x, "x")
		
	ISSUE("jumble need to support primitives")

	parametres(f) <- parametres(f)[c(x)]
	f
}

#' @rdname mchof_parameters
#' @family mchof-parameters
#' @export

mcExplode <- function (f) {
	# takes a function that takes a single value and 
	# makes it into a variadic function

	function (...) {
		f(list(...))
	}
}

#' @rdname mchof_parameters
#' @family mchof-parameters
#' @export

mcImplode <- function (f) {
	# takes a function that takes a many values and 
	# makes it into a function that takes one list

	function (x) {
		do.call(f, c(list(), x))
	}
}

#' @description mcPartial transforms a function that takes multiple arguments
#' into a function that takes less arguments by partially applying the function with
#' the arguments supplied.
#'
#' @title mcPartial
#'  
#' @export
#' @param f a function, or a string giving the name of a function.
#' @param ... name = value pairs to apply to f.
#' @return returns a partially applied function with ellipsis (...) formals
#' 
#' @details mcPartial returns a partially applied function; an example of a partially
#' applied function is 
#' 
#' f(x,y) 2x + y -> f(x) 2x + 2 
#' 
#' @keywords mcPartial
#' @example inst/examples/examples-partial.r

mcPartial <- function (f, ...) {
	# take f and fill in some of its arguments, return a function 
	# with less formals
	
	func_call <- "mcPartial(f, ...)"

	missing(f) %throws% messages$function_is_required(func_call, "f")
	
	ISSUE("partial is very broken")
	
	f <- match.fun(f)
	applied <- list(...)
	
	formals_f <- names(formals(f))
	
	("..." %in% formals_f) %throws% 
		messages$formals_has_ellipses(func_call, formals_f, "f")
	
	( any_unnamed(names(applied)) ) %throws% 
		messages$not_all_named(func_call, applied, "...")
	
	( any_duplicated(names(applied)) ) %throws% 
		messages$matched_muliple_times(func_call, applied, "...")

	rm(formals_f, func_call)

	applied_func <- function () {

		do.call(f, c(applied, as.list(match.call())[-1]) )
	}
	
	formals_f <- names(formals(f))
	unapplied <- formals_f[ !formals_f %in% names(applied) ]
	
	formals(applied_func) <- empty_formals(unapplied)
	applied_func
}
