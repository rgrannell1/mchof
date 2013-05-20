#' @import parallel

call_mclapply <- function (f, x, paropts = NULL) {
	# a wrapper that maps f over x in parallel, and 
	# returns the results. OS-specific implementation.

	if (!is.function(f)) stop('f is not a function')
	if (!is.vector(x)) stop('x is not a vector')
	
	if (.Platform$OS.type == 'windows') {
		message(
			'parallel execution is not currently supported on windows',
			'; executing sequentially')
		return (Map(f,x))
	}
	
	if (!is.null(paropts)) {
		
		arg_names <- names(paropts)		
		valid_formals <- names(formals(parallel::mclapply))

		invalid_args <- setdiff(arg_names, valid_formals)
		
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
		
	} else if (!is.null(getOption('mc.cores'))) {
		paropts <- list(mc.cores = getOption('mc.cores'))
	}	
	
	status <- ''	
	output <- withCallingHandlers({	
		do.call(
			what = parallel::mclapply,
			args = c(list(FUN = f, X = x), paropts))	
		},
		warning = function (w) {
			status <<- 'warning'
		}, error = function (e) {
			status <<- 'error'
	})

	if (status == 'warning') {
		stop ('an mchof function encountered errors:\n', output)
	}
	if (status == 'error') {
		stop ('an mchof function encountered errors:\n', output)
	}
	output	
}
