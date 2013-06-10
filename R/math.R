
#' @description mcLarger takes two functions f and g, and returns a function. This new function
#' returns f(...) > g(...)
#'
#' @title mcLarger
#' 
#' @export
#' @param f a function that returns a number or vector of numbers, 
#' or a string giving the name of such a function.
#' @param g a function that returns a number or vector of numbers, 
#' or a string giving the name of such a function.
#' @return returns a logical value
#' 
#' @keywords mcLarger
#' @example inst/examples/examples-larger.r

mcLarger <- function (f, g) {
	# return a function f(...) > g(...)?
	
	func_call <- paste0( deparse(match.call()), ':' )

	missing(f) %throws% stopf (
		'%s a function (or function name) f is required but was missing',
		func_call)	
	missing(g) %throws% stopf (
		'%s a function (or function name) g is required but was missing',
		func_call)
	
	f <- match.fun(f)
	g <- match.fun(g)
	
	function (...) {
		all(f(...) > g(...))
	}
}

#' @description mcSmaller takes two functions f and g, and returns a function. This new function
#' returns f(...) < g(...)
#'
#' @title mcSmaller
#' 
#' @export
#' @param f a function that returns a number or vector of numbers, 
#' or a string giving the name of such a function.
#' @param g a function that returns a number or vector of numbers, 
#' or a string giving the name of such a function.
#' @return returns a logical value
#' 
#' @keywords mcSmaller
#' @example inst/examples/examples-smaller.r

mcSmaller <- function (f, g) {
	# return a function f(...) < g(...)?
	
	func_call <- paste0( deparse(match.call()), ':' )

	missing(f) %throws% stopf (
		'%s a function (or function name) f is required but was missing',
		func_call)	
	missing(g) %throws% stopf (
		'%s a function (or function name) g is required but was missing',
		func_call)
	
	f <- match.fun(f)
	g <- match.fun(g)
	
	function (...) {
		all(f(...) < g(...))
	}
}

#' @description mcPlus takes two functions f and g, and returns a function. This new function
#' returns f(...) + g(...)
#'
#' @title mcPlus
#' 
#' @export
#' @param f a function that returns a number or vector of numbers, 
#' or a string giving the name of such a function.
#' @param g a function that returns a number or vector of numbers, 
#' or a string giving the name of such a function.
#' @return returns a number or vector of numbers
#' 
#' @keywords mcPlus
#' @example inst/examples/examples-plus.r

mcPlus <- function (f, g) {
	# return a function f(...) + g(...)
	
	func_call <- paste0( deparse(match.call()), ':' )

	missing(f) %throws% stopf (
		'%s a function (or function name) f is required but was missing',
		func_call)	
	missing(g) %throws% stopf (
		'%s a function (or function name) g is required but was missing',
		func_call)
	
	f <- match.fun(f)
	g <- match.fun(g)
	
	function (...) {
		f(...) + g(...)
	}	
}

#' @description mcEqual takes two functions f and g, and returns a function. This new function
#' returns f(...) == g(...)
#'
#' @title mcEqual
#' 
#' @export
#' @param f a function that returns an object that can be compared with ==, 
#' or a string giving the name of such a function.
#' @param g a function that returns an object that can be compared with ==, 
#' or a string giving the name of such a function.
#' @return returns a logical value.
#' 
#' @keywords mcEqual
#' @example inst/examples/examples-equal.r

mcEqual <- function (f, g) {
	# returns a function that returns f(...) == g(...)
	
	func_call <- paste0( deparse(match.call()), ':' )

	missing(f) %throws% stopf (
		'%s a function (or function name) f is required but was missing',
		func_call)	
	missing(g) %throws% stopf (
		'%s a function (or function name) g is required but was missing',
		func_call)
	
	f <- match.fun(f)
	g <- match.fun(g)
	
	function (...) {
		f(...) == g(...)
	}	
}

#' @description mcMinus takes two functions f and g, and returns a function. This new function
#' returns f(...) - g(...)
#'
#' @title mcMinus
#' 
#' @export
#' @param f a function that returns a number or vector of numbers, 
#' or a string giving the name of such a function.
#' @param g a function that returns a number or vector of numbers, 
#' or a string giving the name of such a function.
#' @return returns a number or vector of numbers.
#' 
#' @keywords mcMinus
#' @example inst/examples/examples-minus.r
 

mcMinus <- function (f, g) {
	# get f(...) - g(...)
	
	func_call <- paste0( deparse(match.call()), ':' )

	missing(f) %throws% stopf (
		'%s a function (or function name) f is required but was missing',
		func_call)	
	missing(g) %throws% stopf (
		'%s a function (or function name) g is required but was missing',
		func_call)
	
	f <- match.fun(f)
	g <- match.fun(g)
	
	function (...) {
		f(...) - g(...)
	}	
}

#' @description mcMultiply takes two functions f and g, and returns a function. This new function
#' returns f(...) * g(...)
#'
#' @title mcMultiply
#' 
#' @export
#' @param f a function that returns a number or vector of numbers, 
#' or a string giving the name of such a function.
#' @param g a function that returns a number or vector of numbers, 
#' or a string giving the name of such a function.
#' @return returns a number or vector of numbers.
#' 
#' @keywords mcMultiply
#' @example inst/examples/examples-minus.r

mcMultiply <- function (f, g) {
	# return a function f(...) * g(...)
	
	func_call <- paste0( deparse(match.call()), ':' )

	missing(f) %throws% stopf (
		'%s a function (or function name) f is required but was missing',
		func_call)	
	missing(g) %throws% stopf (
		'%s a function (or function name) g is required but was missing',
		func_call)
	
	f <- match.fun(f)
	g <- match.fun(g)
	
	function (...) {
		f(...) * g(...)
	}	
}

#' @description mcDivide takes two functions f and g, and returns a function. This new function
#' returns f(...) / g(...)
#'
#' @title mcDivide
#' 
#' @export
#' @param f a function that returns a number or vector of numbers, 
#' or a string giving the name of such a function.
#' @param g a function that returns a number or vector of numbers, 
#' or a string giving the name of such a function.
#' @return returns a number or vector of numbers.
#' 
#' @keywords mcDivide
#' @example inst/examples/examples-divide.r

mcDivide <- function (f, g) {
	# return a function f(...) / g(...)?
	
	func_call <- paste0( deparse(match.call()), ':' )

	missing(f) %throws% stopf (
		'%s a function (or function name) f is required but was missing',
		func_call)	
	missing(g) %throws% stopf (
		'%s a function (or function name) g is required but was missing',
		func_call)
	
	f <- match.fun(f)
	g <- match.fun(g)
	
	function (...) {
		f(...) / g(...)
	}	
}
