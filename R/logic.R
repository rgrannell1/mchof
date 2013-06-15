
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
	
	func_call <- "mcAnd(f, g)"

	missing(f) %throws% messages$function_is_required(func_call, "f")
	missing(g) %throws% messages$function_is_required(func_call, "g")
	
	f <- match.fun(f)
	g <- match.fun(g)
	
	rm(func_call)
	
	combine_formals(
		function () {
			f(formals) && g(formals)
		},
		f, g
	)
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
	
	func_call <- "mcNot(f)"

	missing(f) %throws%  messages$function_is_required(func_call, "f")

	f <- match.fun(f)
	
	rm(func_call)
	
	combine_formals(
		function () {
			!f(formals)
		}, f)
}

#' @description mcOr takes two functions f and g, and returns a function. 
#' This new function returns f(...) || g(...)
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
	
	func_call <- "mcOr(f, g)"

	missing(f) %throws%  messages$function_is_required(func_call, "f")
	missing(g) %throws%  messages$function_is_required(func_call, "g")
	
	f <- match.fun(f)
	g <- match.fun(g)
	
	rm(func_call)

	combine_formals(
		function () {
			f(formals) || g(formals)
		}, f, g)
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
	
	func_call <- "mcXor(f, g)"

	missing(f) %throws%  messages$function_is_required(func_call, "f")
	missing(g) %throws%  messages$function_is_required(func_call, "g")	
	
	f <- match.fun(f)
	g <- match.fun(g)
	
	rm(func_call)
	
	combine_formals(
		function () {
			xor( f(formals), g(formals) ) 
		}, f, g)
}
