#' @import parallel

call_mclapply <- function (f, x, paropts = NULL) {
	# a wrapper that maps f over x in parallel, and 
	# returns the results. OS-specific implementation.

	func_call <- if (exists('func_call')) func_call else ''
	
	(!is.function(f)) %throws% stopf (
		'%s f is not a function: actual value was %s (%s)',
		func_call, deparse(f), class(f))

	(!is.vector(x)) %throws% stopf (
		'%s x is not a vector: actual value was %s (%s)',
		func_call, deparse(x), class(x))
	
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

	(status == 'warning') %throws% stopf (
		c('%s', '%s'), func_call, output)
	
	(status == 'error') %throws% stopf (
		c('%s', '%s'), func_call, output)
	
	output	
}
