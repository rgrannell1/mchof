#' @import parallel

once <- function (f) {
	.count <- 0
	function () {
		if (.count < 1) {
			.count <<- .count + 1
			f()
		} else NULL
	}
}

warn_windows <- once(
	function () {
		messages$windows_sequential()		
	}
)

call_mclapply <- function (f, x, paropts = NULL, 
	func_call = "call_mclapply(f, x, paropts)") {
	# a wrapper that maps f over x in parallel, and 
	# returns the results. OS-specific implementation.

	(!is.function(f)) %throws% 
		messages$class_mismatch(func_call, f, "f", "function")
	(!is.vector(x)) %throws% 
		messages$class_mismatch(func_call, f, "f", c("vector", "list"))
	
	if (.Platform$OS.type == 'windows') {
		warn_windows()
		return (lapply(x, f))
	}
	
	if (!is.null(paropts)) {
		
		arg_names <- names(paropts)		
		valid_formals <- names(formals(parallel::mclapply))
		invalid_args <- setdiff(arg_names, valid_formals)
		
		(length(invalid_args) > 0) %throws% 
			messages$invalid_paropts(func_call, invalid_args)
		
		paropts$FUN <- NULL
		paropts$X <- NULL
		
		rm(invalid_args, valid_formals, arg_names)

	} else if (!is.null(getOption('mc.cores'))) {
		paropts <- list(mc.cores = getOption('mc.cores'))
	}

	if (is.null(paropts$mc.cores) || paropts$mc.cores == 1) {
		lapply(x, f)
	} else {
		
		status <- ""
		output <- withCallingHandlers({	
			do.call(
				what = parallel::mclapply,
				args = c(list(FUN = f, X = x), paropts))
			},
			warning = function (warn) {
				status <<- "warning"
			}, error = function (err) {
				status <<- "error"
		})
	
		(status == "warning") %throws% stopf (
			c('%s', '%s'), func_call, output)
		
		(status == "error") %throws% stopf (
			c('%s', '%s'), func_call, output)
		output
	}
}
