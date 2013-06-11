
#' @description mcCurry transforms a function that takes multiple arguments
#' into a function that takes less arguments by partially applying the function.
#'
#' @title mcCurry
#'  
#' @export
#' @param f a function, or a string giving the name of a function.
#' @param ... name = value pairs to curry f with.
#' @return returns a partially applied function with ellipsis (...) formals
#' 
#' @details mcCurry returns a partially applied function; an example of a partially
#' applied function is 
#' 
#' f(x,y) 2x + y -> f(x) 2x + 2 -> 
#' 
#' 
#' @seealso see \code{\link{mcCurryf}} for a currying function that operates on 
#' its formals and performs input validation.
#' 
#' @keywords mcCurry
#' @example inst/examples/examples-curry.r

mcCurry <- function (f, ...) {
	# take f and fill in some of its arguments
	
	func_call <- "mcCurry(f, ...)"
	
	missing(f) %throws% messages$function_is_required(func_call, "f")
	
	f <- match.fun(f)	
	curried <- list(...)
	
	rm(func_call)
	
	function (...) {
		do.call( f, c(curried, list(...)) )
  	}
}

#' @description mcCurryf transforms a function that takes multiple arguments
#' into a function that takes less arguments by partially applying the function.
#' This function differs from mcCurry in that it also alters the formals of its
#' return function.
#'
#' @title mcCurryf
#'  
#' @export
#' @param f a function with no variadic (...) or primitive arguments,
#' or a string giving the name of such a function.
#' @param ... name = value pairs to curry f with.
#' @return returns a partially applied version of f, with modified formals.
#' 
#' @details mcCurryf is superficially similar to mcCurry, but it differs in the kind
#' of function that it returns. mcCurryf modifies the formals parameters of f, 
#' rather than just setting the formals of f to ellipses (...). This allows other
#' functionals to modify the formals of an mcCurryf'd function, but not one 
#' modified by mcCurry.
#' 
#' The downside to this implementation is that functions with variadic 
#' arguments (...) or primitive arguments (base function such as plus) do not
#' work well with mcCurryf.
#' 
#' @seealso see \code{\link{mcCurry}} for a currying function that works with 
#' variadic and primitive formal parameters.
#' 
#' @keywords mcCurryf
#' @example inst/examples/examples-curryf.r

mcCurryf <- function (f, ...) {
	# take f and fill in some of its arguments, return a function 
	# with less formals
	
	func_call <- "mcCurryf(f, ...)"

	missing(f) %throws% function_is_required(func_call, "f")
	
	f <- match.fun(f)
	curried <- list(...)
	
	formals_f <- names(formals(f))
	duplicated_names <- paste0(
		names(curried)[ duplicated(names(curried)) ],
		collapse = ", ")
	
	("..." %in% formals_f) %throws% message$formals_has_ellipses(
		func_call, formals_f, "f")

	(length( which(names(curried) != "") ) < length(curried)) %throws% stopf(
		"%s: not every argument to be curried with f was named", func_call)
	
	(length(unique(names(curried))) != length(curried)) %throws% stopf(
		"%s: some arguments to be curried with f were provided multiple times: %s",
		func_call, duplicated_names)
	
	rm(formals_f, func_call, duplicated_names)
	
	g <- function () {
		do.call(
			what = f,
			args = c(curried, as.list(match.call())[-1]) )
	}
	
	formals_f <- names(formals(f))
	formals_g <- formals_f[ !formals_f %in% names(curried) ]
	empty_symbol <- list(formals(function (x){ })$x)
	
	formals(g) <- structure(
		replicate(n = length(formals_g), expr = empty_symbol),
		names = formals_g)
	g
}


