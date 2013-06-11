
#' @description mcCurry 
#'
#' @title mcCurry
#'  
#' @export
#' @param f a function, or a string giving the name of a function.
#' @param ... args
#' @return returns a function
#' 
#' @keywords mcCurry
#' @example inst/examples/examples-curry.r

mcCurry <- function (f, ...) {
	# take f and fill in some of its arguments
	
	func_call <- paste0( deparse(match.call()), ':' )
	
	missing(f) %throws% stopf (
		'%s a function (or function name) f is required but was missing',
		func_call)
	
	f <- match.fun(f)	
	.curried <- list(...)
	
	rm(func_call)
	
	function (...) {
		do.call( f, c(.curried, list(...)) )
  	}
}

#' @description mcCurryf curries a function, and returns a function with modified
#' formals and the selected parameters curried.
#'
#' @title mcCurryf
#'  
#' @export
#' @param f a function with no variadic (...) or primitive arguments,
#' or a string giving the name of such a function.
#' @param ... name = value pairs to curry f with.
#' @return returns a function with certain parameters curried.
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
	
	func_call <- "mcCurryf(f, ...) "

	missing(f) %throws% stopf (
		'%s: a function (or function name) f is required but was missing',
		func_call)	
	
	f <- match.fun(f)
	curried <- list(...)
	
	formals_f <- names(formals(f))
	
	("..." %in% formals_f) %throws% stopf(
		"%s: ellipses (...) cannot be used in f's formals: actual formals were %s",
		func_call, paste0(formals_f, collapse = ", "))

	(length( which(names(curried) != "") ) < length(curried)) %throws% stopf(
		"%s: not every argument to be curried with f was named", func_call)
	
	(length(unique(names(curried))) != length(curried)) %throws% stopf(
		"%s: some arguments to be curried with f were provided multiple times: %s",
		func_call, 
		paste0(names(curried)[ duplicated(names(curried)) ], collapse = ", "))
	
	rm(formals_f, func_call)
	
	g <- function () {
		do.call( f, c(curried, as.list(match.call())[-1]) )
	}
	
	formals_f <- names(formals(f))
	formals_g <- formals_f[ !formals_f %in% names(curried) ]

	formals(g) <- structure(
		replicate(
			n = length(formals_g),
			expr = list(formals(function (x){ })$x)), # the empty symbol!
		names = formals_g)
	g
}


