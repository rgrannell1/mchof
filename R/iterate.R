
#' @title mcIterateWhile
#' @export

mcIterateWhile <- function (p, f, x) {
	# repeatedly apply f to x, until p of the result is true

	missing(f) %throws% messages$function_is_required(func_call, "f")
	missing(p) %throws% messages$function_is_required(func_call, "p")
	missing(x) %throws% messages$vector_is_required(func_call, "x")

	func_call <- "mcInterateWhile(f, x, paropts = NULL)"

	f <- match.fun(f)
	p <- match.fun(p)

	if (is.null(x)) return (NULL)
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
