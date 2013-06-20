
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
	# (a -> b -> ... -> z) -> (z -> ... -> b -> a)
	# return a function with reversed formal arguments
	
	func_call <- "mcFlip(f)"

	missing(f) %throws% 
		messages$function_is_required(func_call, "f")

	f <- match.fun(f)
	if (length(mcParameters(f)) < 2) return (f)

	mcParameters(f, rev(mcParameters(f)))
}

#' @rdname mchof_parameters
#' @family mchof-parameters
#' @export

mcJumble <- function (f, x) {
	# function -> function 
	# returns a function with its formals rearranged, 
	# according to a index vector (bijection) 
	
	func_call <- "mcJumble(f, x)"
	
	missing(f) %throws% 
		messages$function_is_required(func_call, "f")
	missing(x) %throws% 
		messages$vector_is_required(func_call, "x")

	is.factor(x) %throws% 
		messages$was_a_factor(func_call, x, "x")
	
	f <- match.fun(f)
	if (length(parameters(f)) < 2) return (f)
	
	( !all(x %in% seq_along(parameters(f))) ) %throws% 
		messages$must_be_indices(func_call, x, "x")
	
	(length(parameters(f)) != length(x)) %throws% 
		messages$length_mismatch(
			func_call, c(length(parameters(f)),
			length(x)), "parameters(f)", "x")

	(any_duplicated(x)) %throws% 
		messages$matched_multiple_time(func_call, x, "x")

	mcParameters(
		f,
		mcParameters(f)[c(x)])
}

#' @rdname mchof_parameters
#' @family mchof-parameters
#' @export

mcParameters <- function (f, x) {
	# (a -> b -> ... -> z) -> [a, b, ..., z]
	# (a -> b -> ... -> z) -> [x1, x2, ..., xn] -> (x1 -> x2 -> ... -> xn)
	# get the formals/arguments of f if x
	# isn't given, and set the formals if x is given

	func_call <- "mcParameters(f, x)"

	missing(f) %throws% 
		messages$function_is_required(func_call, "f")

	f <- match.fun(f)

	use_as_getter <- missing(x)

	if (use_as_getter) {

		parameters <- if (is.primitive(f)) {
			head( as.list(args(f)), -1 )
		} else {
			formals(f)
		}
		return (parameters)
	}

	if (is.primitive(f)) {
		# prime to work with setter 

		g <- function () {
			do.call(f, )
		}
	}

	is_correct_class <- 
		(is.vector(x) && is.character(x)) || is.list(x) 

	(!is_correct_class) %throws%
		messages$cant_be_parameters(func_call, x, "x")
 
	if (is.vector(x) && is.character(x)) {

		any_unnamed(x) %throws% 
			messages$not_all_named(func_call, x, "x")

		any_duplicated(x) %throws%
			messages$matched_multiple_time(func_call, x, "x")

		missing_default <- list( formals(function (x) { })$x )

		if (length(x) > 0) {

			formals(f) <- structure(
				replicate(length(x), missing_default),
				names = x)
			f
		} else {
			formals(f) <- list()
			f
		}
	}

	if (is.list(x)) {

		(any_unnamed(x)) %throws% 
			messages$not_all_named(func_call, x, "x")
	
		formals(f) <- x
		return (f)
	}
}

#' @rdname mchof_parameters
#' @family mchof-parameters
#' @export

mcExplode <- function (f) {
	# (a -> b) -> (... -> b)
	# takes a function that takes a single value and 
	# makes it into a variadic function

	func_call <- "mcExplode(f)"

	missing(f) %throws% 
		messages$function_is_required(func_call, "f")
	f <- match.fun(f)

	function (...) {
		f(list(...))
	}
}

#' @rdname mchof_parameters
#' @family mchof-parameters
#' @export

mcImplode <- function (f) {
	# (... -> b) -> (a -> b)
	# dual to mcExplode. 
	# takes a function that takes a many values and 
	# makes it into a function that takes one list

	func_call <- "mcImplode(f)"

	missing(f) %throws% 
		messages$function_is_required(func_call, "f")
	f <- match.fun(f)

	function (x) {
		do.call(f, c(list(), x))
	}
}

#' @rdname mchof_parameters
#' @family mchof-parameters
#' @export

mcPartial <- function (f, ...) {
	# take f and fill in some of its arguments, return a function 
	# with less formals
	
	func_call <- "mcPartial(f, ...)"

	missing(f) %throws% 
		messages$function_is_required(func_call, "f")
	
	f <- match.fun(f)
	applied <- list(...)
	
	formals_f <- names(mcParameters(f))
	
	("..." %in% formals_f) %throws% 
		messages$formals_has_ellipses(func_call, formals_f, "f")
	
	( any_unnamed(applied) ) %throws% 
		messages$not_all_named(func_call, applied, "...")
	
	( any_duplicated(names(applied)) ) %throws% 
		messages$matched_muliple_times(func_call, applied, "...")

	rm(formals_f, func_call)

	applied_func <- function () {

		do.call(f, c(applied, as.list(match.call())[-1]) )
	}
	
	formals_f <- names(mcParameters(f))
	unapplied <- formals_f[ !formals_f %in% names(applied) ]
	
	ISSUE("partial is very broken")
	
	formals(applied_func) <- empty_formals(unapplied)
	applied_func
}

mcCurry <- function (f) {
	# take a function f that can take multiple multiple 
	# arguments and transform it into a chain of single variable
	# functions

	
}