#'
#' Map-like Higher-Order-Functions
#'
#' @details 1
#'
#' @keywords mcIterateWhile, mcIndMap
#'
#' @example inst/examples/examples-maps.r
#'
#' @param p 1
#' @param f 1
#' @param x x
#' @param paropts 1
#'
#'
#'
#'
#' @rdname mchof_maps
#' @family mchof_maps
#' @export

mcIterateWhile <- function (p, f, x) {
	# repeatedly apply f to x, until p of the result is true
	
	func_call <- "mcInterateWhile(f, x, paropts = NULL)"

	missing(f) %throws% messages$function_is_required(func_call, "f")
	missing(p) %throws% messages$function_is_required(func_call, "p")
	missing(x) %throws% messages$vector_is_required(func_call, "x")

	f <- match.fun(f)
	p <- match.fun(p)

	if (length(x) == 0) return (x)
	(!is.vector(x)) %throws% 
		messages$class_mismatch(func_call, x, "x", "vector or list")

	repeat {

		result <- p(x)

		(!is_boolean(result)) %throws% 
			messages$class_mismatch(func_call, result, "p(x)", "TRUE/FALSE value")

		if (!result) break

		x <- f(x)
	}
	x
}

#' @rdname mchof_maps
#' @family mchof-maps
#' @export

mcIterate <- function (f, x) {
	mcIterateWhile(Negate(is.null), f, x)
}

#' @rdname mchof_maps
#' @family mchof-maps
#' @export

mcIndMap <- function (f, x, paropts = NULL) {
	# map f across the list [ [x1_i, x1], ..., [xn_i, xn] ]
	
	func_call <- "mcIndMap(f, x, paropts = NULL)"

	missing(f) %throws% messages$function_is_required(func_call, "f")
	missing(x) %throws% messages$vector_is_required(func_call, "x")

	f <- match.fun(f)

	if (length(x) == 0) return (x)
	(!is.vector(x)) %throws% 
		messages$class_mismatch(func_call, x, "x", "vector or list")

	call_mclapply(
		function (ind) {
			f( x[[ind]], ind )
		},
		seq_along(x),
		paropts
	)
}
