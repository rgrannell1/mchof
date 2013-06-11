#' @import parallel

.mchof_windows_warned <- FALSE

call_mclapply <- function (f, x, paropts = NULL) {
	# a wrapper that maps f over x in parallel, and 
	# returns the results. OS-specific implementation.

	func_call <- if (exists('func_call')) func_call else ''
	par_mclapply <- parallel::mclapply
	
	(!is.function(f)) %throws% stopf (
		'%s f is not a function: actual value was %s (%s)',
		func_call, deparse(f), paste0(class(x), collapse = ', '))

	(!is.vector(x)) %throws% stopf (
		'%s x is not a vector: actual value was %s (%s)',
		func_call, deparse(x), paste0(class(x), collapse = ', '))
	
	if (.Platform$OS.type == 'windows') {			
		if (!.mchof_windows_warned) {
			# this seems kludgy, but it works for the foreach package

			msg <- sample(
				c(paste0("parallel execution is not supported on windows: ",
					"\n", "executing on one core"),
				paste0("parallel execution is not supported on windows:",
					"\n", "executing on one core :/")),
				prob = c(0.8, 0.2), size = 1)
			
			warning(msg, call. = FALSE)
			.mchof_windows_warned <<- TRUE
		}
		return (lapply(x, f))
	}
	
	if (!is.null(paropts)) {
		
		arg_names <- names(paropts)		
		valid_formals <- names(formals(par_mclapply))

		invalid_args <- setdiff(arg_names, valid_formals)
		
		(length(invalid_args) > 0) %throws% stopf(
			'invalid arguments given to paropts: %s', 
				 paste(invalid_args, collapse = ', '))
		
		paropts$FUN <- NULL
		paropts$X <- NULL

	} else if (!is.null(getOption('mc.cores'))) {
		paropts <- list(mc.cores = getOption('mc.cores'))
	}

	if (is.null(paropts$mc.cores) || paropts$mc.cores == 1) {
		lapply(x, f)
	} else {
		status <- ""	
		output <- withCallingHandlers({	
			do.call(
				what = par_mclapply,
				args = c(list(FUN = f, X = x), paropts))
			},
			warning = function (w) {
				status <<- "warning"
			}, error = function (e) {
				status <<- "error"
		})
	
		(status == "warning") %throws% stopf (
			c('%s', '%s'), func_call, output)
		
		(status == "error") %throws% stopf (
			c('%s', '%s'), func_call, output)
		output
	}
}
