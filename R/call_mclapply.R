#' @import parallel

call_mclapply <- function (f, x, paropts = NULL) {
	# provides the interface to the parallel backend for 
	# other functions in mchof

	if (!is.function(f)) stop('f is not a function')
	if (!is.vector(x)) stop('x is not a vector')
	
	if (!is.null(paropts)) {
		
		arg_names <- names(paropts)
		invalid_args <- setdiff(
			arg_names, 
			names(formals(mclapply)))
		
		if (length(invalid_args) > 0) {
			stop('invalid arguments given to paropts: ', 
				 paste(invalid_args, collapse = ','))
		}
		if (any(c('f', 'FUN') %in% names(paropts))) {
			stop('f or FUN may not be specified in paropts')
		}
		if (any(c('x', 'X') %in% names(paropts))) {
			stop('x or X may not be specified in paropts')
		}	
		
	}
	
	do.call(
		what = parallel::mclapply,
		args = c(list(FUN = f, X = x), paropts))
}