#' @import parallel

call_mclapply <- function (f, x, paropts = NULL) {
	# a wrapper segregating the parallel library from mchof code
	
	if (!is.function(f)) stop('f is not a function')
	if (!is.vector(x)) stop('x is not a vector')
	
	if (!is.null(paropts)) {
		
		arg_names <- names(paropts)
		
		invalid_args <- setdiff(
			arg_names, 
			names(formals(parallel::mclapply)))
		
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
		
	} else {
		if (!is.null(getOption('mc.cores'))) {
			paropts <- list(mc.cores = getOption('mc.cores'))
		}	
	}
	status <- list (value = '')
	
	output <- withCallingHandlers({
		do.call(
			what = parallel::mclapply,
			args = c(list(FUN = f, X = x), paropts))	
		},
		warning = function (w) {
			status <<- list (value = 'warning')
		}, error = function (e) {
			status <<- list (value = 'error')
	})

	if (status$value == 'warning') {
		stop ('an mchof function encountered errors:\n', 
			output)
	}
	if (status$value == 'error') {
		stop ('an mchof function encountered errors:\n', 
			  output)
	}
	output
	
}



