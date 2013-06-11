
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
	
	local({
		func_call <- paste0( deparse(match.call()), ':' )
	
		missing(f) %throws% stopf (
			'%s a function (or function name) f is required but was missing',
			func_call)
	})
	
	f <- match.fun(f)		
	.curried_arguments <- list(...)
	
	function (...) {
		do.call( f, c(.curried_arguments, list(...)) )
  	}
}

#' @description mcCurryf is a form of currying involving more rigorous validation
#' of input arguments, at the cost of not supporting variadic functions.
#'
#' @title mcCurryf
#'  
#' @export
#' @param f a function with no variadic (...) or primitive arguments,
#' or a string giving the name of such a function.
#' @param ...
#' @return returns a function
#' 
#' @keywords mcCurryf
#' @example inst/examples/examples-curryf.r

mcCurryf <- function (f, ...) {
	# take f and fill in some of its arguments
	
	func_call <- paste0( deparse(match.call()), ':' )

	missing(f) %throws% stopf (
		'%s a function (or function name) f is required but was missing',
		func_call)	
	
	f <- match.fun(f)
	args <- list(...)
	
	formals_f <- names(formals(f))
}

