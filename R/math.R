
#' Higher-Order-Functions for Functional Arithmetic
#'
#' @description
#'
#'\code{mcLarger} takes two functions \code{f} and \code{g}, and returns a composite function.
#' this composite function returns \code{f(...) > g(...)}
#'
#'\code{mcSmaller} takes two functions \code{f} and \code{g}, and returns a composite function.
#' this composite function returns \code{f(...) < g(...)}
#''
#'\code{mcPlus} takes two functions \code{f} and \code{g}, and returns a composite function.
#' this composite function returns \code{f(...) + g(...)}
#'
#'\code{mcMinus} takes two functions \code{f} and \code{g}, and returns a composite function.
#' this composite function returns \code{f(...) - g(...)}
#'
#'\code{mcEqual} takes two functions \code{f} and \code{g}, and returns a composite function.
#' this composite function returns \code{f(...) == g(...)}
#'
#'\code{mcNotEqual} takes two functions \code{f} and \code{g}, and returns a composite function.
#' this composite function returns \code{f(...) != g(...)}
#'
#'\code{mcMultiply} takes two functions \code{f} and \code{g}, and returns a composite function.
#' this composite function returns \code{f(...) * g(...)}
#'
#'\code{mcDivide} takes two functions \code{f} and \code{g}, and returns a composite function.
#' this composite function returns \code{f(...) / g(...)}
#'
#' @details 1
#'
#' @param f a function that returns a value, or a string giving the name of such a function.
#' @param g a function that returns a value, or a string giving the name of such a function.
#'
#' @rdname mchof_math
#' @family mchof-math
#' @example inst/examples/examples-math.r
#' @export

mcLarger <- function (f, g) {
	# return a function f(...) > g(...)?
	
	func_call <- "mcLarger(f, g)"

	missing(f) %throws% messages$function_is_required(func_call, "f")
	missing(g) %throws% messages$function_is_required(func_call, "g")		

	f <- match.fun(f)
	g <- match.fun(g)
	
	rm(func_call)
	
	combine_formals(
		function () {			
			f(params_) > g(params_)
		}, f, g)
}

#' @rdname mchof_math
#' @family mchof-math
#' @export

mcSmaller <- function (f, g) {
	# return a function f(...) < g(...)?
	
	func_call <- "mcSmaller(f, g)"

	missing(f) %throws% messages$function_is_required(func_call, "f")
	missing(g) %throws% messages$function_is_required(func_call, "g")	

	f <- match.fun(f)
	g <- match.fun(g)
	
	rm(func_call)
	
	combine_formals(
		function () {			
			f(params_) < g(params_)
		}, f, g)
}

#' @rdname mchof_math
#' @family mchof-math
#' @export

mcPlus <- function (f, g) {
	# return a function f(...) + g(...)
	
	func_call <- "mcPlus(f, g)"

	missing(f) %throws% messages$function_is_required(func_call, "f")
	missing(g) %throws% messages$function_is_required(func_call, "g")		

	f <- match.fun(f)
	g <- match.fun(g)
	
	rm(func_call)
	
	combine_formals(
		function () {			
			f(params_) + g(params_)
		}, f, g)
}

#' @rdname mchof_math
#' @family mchof-math
#' @export

mcMinus <- function (f, g) {
	# get f(...) - g(...)
	
	func_call <- "mcMinus(f, g)"

	missing(f) %throws% messages$function_is_required(func_call, "f")
	missing(g) %throws% messages$function_is_required(func_call, "g")	
	
	f <- match.fun(f)
	g <- match.fun(g)
	
	rm(func_call)

	combine_formals(
		function () {			
			f(params_) - g(params_)
		}, f, g)
}

#' @rdname mchof_math
#' @family mchof-math
#' @export

mcEqual <- function (f, g) {
	# returns a function that returns f(...) == g(...)
	
	func_call <- "mcEqual(f, g)"

	missing(f) %throws% messages$function_is_required(func_call, "f")
	missing(g) %throws% messages$function_is_required(func_call, "g")

	f <- match.fun(f)
	g <- match.fun(g)
	
	rm(func_call)
	
	combine_formals(
		function () {			
			f(params_) == g(params_)
		}, f, g)
}

#' @rdname mchof_math
#' @family mchof-math mchof-logic
#' @export

mcNotEqual <- function (f, g) {
	# returns a function that returns f(...) == g(...)
	
	func_call <- "mcNotEqual(f, g)"

	missing(f) %throws% messages$function_is_required(func_call, "f")
	missing(g) %throws% messages$function_is_required(func_call, "g")		

	f <- match.fun(f)
	g <- match.fun(g)
	
	rm(func_call)
	
	combine_formals(
		function () {			
			f(params_) != g(params_)
		}, f, g)
}

#' @rdname mchof_math
#' @family mchof-math mchof-logic
#' @export

mcMultiply <- function (f, g) {
	# return a function f(...) * g(...)

	func_call <- "mcMultiply(f, g)"

	missing(f) %throws% messages$function_is_required(func_call, "f")
	missing(g) %throws% messages$function_is_required(func_call, "g")
	
	f <- match.fun(f)
	g <- match.fun(g)
	
	rm(func_call)
	
	combine_formals(
		function () {			
			f(params_) * g(params_)
		}, f, g)
}

#' @rdname mchof_math
#' @family mchof-math
#' @export

mcDivide <- function (f, g) {
	# return a function f(...) / g(...)?
	
	func_call <- "mcDivide(f, g)"

	missing(f) %throws% messages$function_is_required(func_call, "f")
	missing(g) %throws% messages$function_is_required(func_call, "g")	
	
	f <- match.fun(f)
	g <- match.fun(g)
	
	rm(func_call)
	
	combine_formals(
		function () {			
			f(params_) / g(params_)
		}, f, g)
}
