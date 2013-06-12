
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
	
	func_call <- "mcLarger(f, g)"

	missing(f) %throws% messages$function_is_required(func_call, "f")
	missing(g) %throws% messages$function_is_required(func_call, "g")		

	f <- match.fun(f)
	g <- match.fun(g)
	
	rm(func_call)
	
	func <- function (...) {
		all(f(...) > g(...))
	}
	set_formals(func, f, g)
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
	
	func_call <- "mcSmaller(f, g)"

	missing(f) %throws% messages$function_is_required(func_call, "f")
	missing(g) %throws% messages$function_is_required(func_call, "g")	

	f <- match.fun(f)
	g <- match.fun(g)
	
	rm(func_call)
	
	func <- function (...) {
		all(f(...) < g(...))
	}
	set_formals(func, f, g)
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
	
	func_call <- "mcPlus(f, g)"

	missing(f) %throws% messages$function_is_required(func_call, "f")
	missing(g) %throws% messages$function_is_required(func_call, "g")		

	f <- match.fun(f)
	g <- match.fun(g)
	
	rm(func_call)
	
	func <- function (...) {
		f(...) + g(...)
	}	
	set_formals(func, f, g)
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
	
	func_call <- "mcEqual(f, g)"

	missing(f) %throws% messages$function_is_required(func_call, "f")
	missing(g) %throws% messages$function_is_required(func_call, "g")

	f <- match.fun(f)
	g <- match.fun(g)
	
	rm(func_call)
	
	func <- function (...) {
		f(...) == g(...)
	}	
	set_formals(func, f, g)
}

#' @description mcNotEqual takes two functions f and g, and returns a function. This new function
#' returns f(...) != g(...)
#'
#' @title mcNotEqual
#' 
#' @export
#' @param f a function that returns an object that can be compared with ==, 
#' or a string giving the name of such a function.
#' @param g a function that returns an object that can be compared with ==, 
#' or a string giving the name of such a function.
#' @return returns a logical value.
#' 
#' @keywords mcNotEqual
#' @example inst/examples/examples-notequal.r

mcNotEqual <- function (f, g) {
	# returns a function that returns f(...) == g(...)
	
	func_call <- "mcNotEqual(f, g)"

	missing(f) %throws% messages$function_is_required(func_call, "f")
	missing(g) %throws% messages$function_is_required(func_call, "g")		

	f <- match.fun(f)
	g <- match.fun(g)
	
	rm(func_call)
	
	func <- function (...) {
		f(...) != g(...)
	}
	set_formals(func, f, g)
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
	
	func_call <- "mcMinus(f, g)"

	missing(f) %throws% messages$function_is_required(func_call, "f")
	missing(g) %throws% messages$function_is_required(func_call, "g")	
	
	f <- match.fun(f)
	g <- match.fun(g)
	
	rm(func_call)

	func <- function (...) {
		f(...) - g(...)
	}	
	set_formals(func, f, g)
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

	func_call <- "mcMultiply(f, g)"

	missing(f) %throws% messages$function_is_required(func_call, "f")
	missing(g) %throws% messages$function_is_required(func_call, "g")
	
	f <- match.fun(f)
	g <- match.fun(g)
	
	rm(func_call)
	
	func <- function (...) {
		f(...) * g(...)
	}	
	set_formals(func, f, g)
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
	
	func_call <- "mcDivide(f, g)"

	missing(f) %throws% messages$function_is_required(func_call, "f")
	missing(g) %throws% messages$function_is_required(func_call, "g")	
	
	f <- match.fun(f)
	g <- match.fun(g)
	
	rm(func_call)
	
	func <- function (...) {
		f(...) / g(...)
	}
	set_formals(func, f, g)
}

