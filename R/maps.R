
#' @title mcIterateWhile
#' @export
#' @keywords mcIterateWhile
#'
#' @param p a function that takes an element of code{{f(x), f(f(x)), f(f(f(x))), ...}} and returns a TRUE/FALSE value
#' @param f a function that is repeatedly applied to \code{x, f(x), f(f(x)), ...}
#' @param x an initial vector/list to apply f to repeatedly.
#'

mcIterateWhile <- function (p, f, x) {
	# repeatedly apply f to x, until p of the result is true
	
	func_call <- "mcInterateWhile(f, x, paropts = NULL)"

	missing(f) %throws% messages$function_is_required(func_call, "f")
	missing(p) %throws% messages$function_is_required(func_call, "p")
	missing(x) %throws% messages$vector_is_required(func_call, "x")

	f <- match.fun(f)
	p <- match.fun(p)

	if (length(x) == 0) return (x)
	is.factor(x) %throws% messages$was_factor(func_call, x, "x")

	repeat {

		result <- p(x)

		(!is_boolean(result)) %throws% 
			messages$not_a_bool(func_call, result, "p(x)")

		if (!result) break

		x <- f(x)
	}
	x
}

#' @title mcIndMap
#' @export
#' @keywords mcIndMap
#'
#' @param f 
#' @param x 
#' @param paropts 

mcIndMap <- function (f, x, paropts = NULL) {
	# map f across the list [ [x1_i, x1], ..., [xn_i, xn] ]
	
	func_call <- "mcIndMap(f, x, paropts = NULL)"

	missing(f) %throws% messages$function_is_required(func_call, "f")
	missing(x) %throws% messages$vector_is_required(func_call, "x")

	f <- match.fun(f)

	if (length(x) == 0) return (x)
	is.factor(x) %throws% messages$was_factor(func_call, x, "x")

	call_mclapply(
		function (pair) {
			f( pair[[1]], pair[[2]] )
		},
		mcZip(x, seq_along(x)),
		paropts
	)
}
