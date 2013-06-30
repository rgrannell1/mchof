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
