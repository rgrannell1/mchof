
#' Map a binary function over a collection and its indices
#'
#' @param f a binary function, 

mcIndMap <- function (f, xs, paropts = NULL) {
	# map f across the list [ [x1_i, x1], ..., [xn_i, xn] ]
	
	pcall <- sys.call()

	require_a("functionable", f, pcall)
	require_a("listy", xs, pcall)
	require_a(c("named list", "named pairlist"), paropts, pcall)

	f <- match.fun(f)
	require_a('binary function', f, pcall)

	if (length(xs) == 0) {
		xs
	} else {
		call_mclapply(
			function (ind) {
				f( xs[[ind]], ind )
			},
			seq_along(xs),
			paropts, pcall)		
	}
}
