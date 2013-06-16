
#' @title mcIterateWhile
#' @export
#' @keywords mcIterateWhile
#'
#' @param p a function that takes an element of code{{f(x), f(f(x)), f(f(f(x))), ...}} and returns a TRUE/FALSE value
#' @param f a function that is repeatedly applied to \code{x, f(x), f(f(x)), ...}
#' @param x an initial vector/list to apply f to.
#'

mcIterateWhile <- function (p, f, x) {
	# repeatedly apply f to x, until p of the result is true

	missing(f) %throws% messages$function_is_required(func_call, "f")
	missing(p) %throws% messages$function_is_required(func_call, "p")
	missing(x) %throws% messages$vector_is_required(func_call, "x")

	func_call <- "mcInterateWhile(f, x, paropts = NULL)"

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
