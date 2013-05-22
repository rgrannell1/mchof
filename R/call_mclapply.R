#' @import parallel

call_mclapply <- function (f, x, paropts = NULL) {
	# a wrapper that maps f over x in parallel, and 
	# returns the results. OS-specific implementation.

	(!is.function(f)) %throws% stop('f is not a function')
	(!is.vector(x)) %throws% stop('x is not a vector')
	
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
		
		(length(invalid_args) > 0) %throws% stop(
			'invalid arguments given to paropts: ', 
				 paste(invalid_args, collapse = ','))
		
		( any(c('f', 'FUN') %in% names(paropts)) ) %throws%
			stop('f or FUN may not be specified in paropts')
	
		( any(c('x', 'X') %in% names(paropts)) ) %throws%
			stop('x or X may not be specified in paropts')
	
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

	(status == 'warning') %throws%
		stop ('an mchof function encountered errors:\n', output)
	
	(status == 'error') %throws%
		stop ('an mchof function encountered errors:\n', output)
	
	output	
}
