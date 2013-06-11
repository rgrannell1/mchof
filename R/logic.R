
#' @description mcAnd takes two functions f and g, and returns a function. This new function
#' returns f(...) && g(...)
#'
#' @title mcAnd
#' 
#' @export
#' @param f a function that returns a logical value, or a string giving the name of 
#' such a function.
#' @param g a function that returns a logical value, or a string giving the name of 
#' such a function.
#' @return returns a logical value.
#'  
#' @seealso see \code{\link{mcOr}}, \code{\link{mcNot}} and
#'  \code{\link{mcXor}} for other logical functionals in mchof.
#' 
#' @keywords mcAnd
#' @example inst/examples/examples-and.r

mcAnd <- function (f, g) {
	# return a function that returns true
	# if (f(...) && g(...))
	
	local({
		func_call <- paste0( deparse(match.call()), ':' )

		missing(f) %throws% stopf (
			'%s a function (or function name) f is required but was missing',
			func_call)	
		missing(g) %throws% stopf (
			'%s a function (or function name) g is required but was missing',
			func_call)	
	})

	f <- match.fun(f)
	g <- match.fun(g)
	
	function (...) {
		f(...) && g(...)
	}
}

#' @description mcNot takes a function f, and negates its logical output.
#'
#' @title mcNot
#' 
#' @export
#' @param f a function that returns a logical value, or a string giving the name of 
#' such a function.
#' @return returns a function that returns false when f returns true, 
#' true when f returns false and na when f returns na.
#'  
#' @seealso see \code{\link{mcAnd}}, \code{\link{mcOr}}, and
#'  \code{\link{mcXor}} for other logical functionals in mchof
#' 
#' @keywords mcNot
#' @example inst/examples/examples-not.r

mcNot <- function (f) {
	# return a function that returns false when f is true, 
	# true when f is false, na when na
	
	local({
		func_call <- paste0( deparse(match.call()), ':' )
	
		missing(f) %throws% stopf (
			'%s a function (or function name) f is required but was missing',
			func_call)		
	})

	f <- match.fun(f)
	function (...) !f(...)
}

#' @description mcOr takes two functions f and g, and returns a function. This new function
#' returns f(...) || g(...)
#'
#' @title mcOr
#' 
#' @export
#' @param f a function that returns a logical value, or a string giving the name of 
#' such a function.
#' @param g a function that returns a logical value, or a string giving the name of 
#' such a function.
#' @return returns a logical value.
#'  
#' @seealso see \code{\link{mcAnd}}, \code{\link{mcNot}} and \code{\link{mcXor}},
#' for other logical functionals in mchof.
#' 
#' @keywords mcOr
#' @example inst/examples/examples-or.r

mcOr <- function (f, g) {
	# return a function that returns true
	# if (f(...) || g(...))
	
	local({
		func_call <- paste0( deparse(match.call()), ':' )
	
		missing(f) %throws% stopf (
			'%s a function (or function name) f is required but was missing',
			func_call)	
		missing(g) %throws% stopf (
			'%s a function (or function name) g is required but was missing',
			func_call)		
	})

	f <- match.fun(f)
	g <- match.fun(g)
	
	function (...) {
		f(...) || g(...)
	}
}

#' @description mcXor takes two functions f and g, and returns a function. This new function
#' returns \code{xor( f(...), g(...) )}
#'
#' @title mcXor
#' 
#' @export
#' @param f a function that returns a logical value, or a string giving the name of 
#' such a function.
#' @param g a function that returns a logical value, or a string giving the name of 
#' such a function.
#' @return returns a logical value.
#'  
#' @seealso see \code{\link{mcAnd}}, \code{\link{mcOr}}, and \code{\link{mcNot}} ,
#' for other logical functionals in mchof.
#' @keywords mcXor
#' @example inst/examples/examples-xor.r

mcXor <- function (f, g) {
	# return a function that returns true
	# if (f(...) xor g(...))
	
	local({
		func_call <- paste0( deparse(match.call()), ':' )
	
		missing(f) %throws% stopf (
			'%s a function (or function name) f is required but was missing',
			func_call)	
		missing(g) %throws% stopf (
			'%s a function (or function name) g is required but was missing',
			func_call)		
	})
	
	f <- match.fun(f)
	g <- match.fun(g)
	
	function (...) {
		xor(f(...), g(...))
	}
}
