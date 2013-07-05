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
	
	pcall <- sys.call(sys.parent())

	require_a(c('function', 'string'), f, pcall)
	require_a(c('vector', 'pairlist', 'null'), x, pcall)
	
	f <- match.fun(f)
	
	require_a('binary function', f, pcall)

	if (length(x) == 0) return (x)

	call_mclapply(
		function (ind) {
			f( x[[ind]], ind )
		},
		seq_along(x),
		paropts, pcall)
}
