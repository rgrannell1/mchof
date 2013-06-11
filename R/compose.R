
#' @description mcCompose takes two functions f and g, and returns a function. 
#' This new function returns f(g(...))
#'
#' @title mcCompose
#' @aliases %of%
#'  
#' @usage mcCompose(f, g)
#' 
#' f %of% g
#' 
#' @export
#' @param f a function, or a string giving the name of a function.
#' @param g a function, or a string giving the name of a function.
#' @return returns a function that returns f(g(...))
#' 
#' @keywords mcCompose
#' @example inst/examples/examples-compose.r

mcCompose <- function (f, g) {
	# return a composite function of f and g
	
	func_call <- "mcCompose(f, g)"

	missing(f) %throws% messages$function_is_required(func_call, "f")
	missing(g) %throws% messages$function_is_required(func_call, "g")
	
	f <- match.fun(f)
	g <- match.fun(g)
	
	rm(func_call)
	
	function (...) {
		f(g(...))
	}
}

#' @export

'%of%' <- mcCompose
