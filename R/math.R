
#' @description mcLarger takes two functions f and g, and returns a function. This new function
#' returns f(...) > g(...)
#'
#' @title mcLarger
#' 
#' @param f a function that returns a number or vector of numbers, 
#' or a string giving the name of such a function.
#' @param g a function that returns a number or vector of numbers, 
#' or a string giving the name of such a function.
#' @return returns a logical value
#' 
#' @template roxygen-math
#' @template roxygen-formals
#'
#' @keywords mcLarger
#' @example inst/examples/examples-larger.r
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

#' @description mcSmaller takes two functions f and g, and returns a function. This new function
#' returns f(...) < g(...)
#'
#' @title mcSmaller
#' 
#' @param f a function that returns a number or vector of numbers, 
#' or a string giving the name of such a function.
#' @param g a function that returns a number or vector of numbers, 
#' or a string giving the name of such a function.
#' @return returns a logical value
#' 
#' @template roxygen-math
#'
#' @keywords mcSmaller
#' @example inst/examples/examples-smaller.r
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

#' @description mcPlus takes two functions f and g, and returns a function. This new function
#' returns f(...) + g(...)
#'
#' @title mcPlus
#' 
#' @param f a function that returns a number or vector of numbers, 
#' or a string giving the name of such a function.
#' @param g a function that returns a number or vector of numbers, 
#' or a string giving the name of such a function.
#' @return returns a number or vector of numbers
#'
#' @template roxygen-math
#' @template roxygen-formals
#'
#' @keywords mcPlus
#' @example inst/examples/examples-plus.r
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

#' @description mcEqual takes two functions f and g, and returns a function. This new function
#' returns f(...) == g(...)
#'
#' @title mcEqual
#' 
#' @param f a function that returns an object that can be compared with ==, 
#' or a string giving the name of such a function.
#' @param g a function that returns an object that can be compared with ==, 
#' or a string giving the name of such a function.
#' @return returns a logical value.
#'
#' @template roxygen-math
#' @template roxygen-formals
#'
#' @keywords mcEqual
#' @example inst/examples/examples-equal.r
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

#' @description mcNotEqual takes two functions f and g, and returns a function. This new function
#' returns f(...) != g(...)
#'
#' @title mcNotEqual
#' 
#' @param f a function that returns an object that can be compared with ==, 
#' or a string giving the name of such a function.
#' @param g a function that returns an object that can be compared with ==, 
#' or a string giving the name of such a function.
#' @return returns a logical value.
#'
#' @template roxygen-math
#' @template roxygen-formals
#'
#' @keywords mcNotEqual
#' @example inst/examples/examples-notequal.r
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

#' @description mcMinus takes two functions f and g, and returns a function. This new function
#' returns f(...) - g(...)
#'
#' @title mcMinus
#' 
#' @param f a function that returns a number or vector of numbers, 
#' or a string giving the name of such a function.
#' @param g a function that returns a number or vector of numbers, 
#' or a string giving the name of such a function.
#' @return returns a number or vector of numbers.
#'
#' @template roxygen-math
#' @template roxygen-formals
#'
#' @keywords mcMinus
#' @example inst/examples/examples-minus.r
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

#' @description mcMultiply takes two functions f and g, and returns a function. This new function
#' returns f(...) * g(...)
#'
#' @title mcMultiply
#' 
#' @param f a function that returns a number or vector of numbers, 
#' or a string giving the name of such a function.
#' @param g a function that returns a number or vector of numbers, 
#' or a string giving the name of such a function.
#' @return returns a number or vector of numbers.
#'
#' @template roxygen-math
#' @template roxygen-formals
#'
#' @keywords mcMultiply
#' @example inst/examples/examples-minus.r
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

#' @description mcDivide takes two functions f and g, and returns a function. This new function
#' returns f(...) / g(...)
#'
#' @title mcDivide
#' 
#' @param f a function that returns a number or vector of numbers, 
#' or a string giving the name of such a function.
#' @param g a function that returns a number or vector of numbers, 
#' or a string giving the name of such a function.
#' @return returns a number or vector of numbers.
#'
#' @template roxygen-math
#' @template roxygen-formals
#'
#' @keywords mcDivide
#' @example inst/examples/examples-divide.r
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

