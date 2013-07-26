#' @import parallel

warn_windows_users <- ( function () {
	"warn unfortunate windows users that 
	forking parallel is not available on their platform"

	once <- function (f) {
		.count <- 0
		function () {
			if (.count < 1) {
				.count <<- .count + 1
				f()
			} else {
				NULL
			}
		}
	}

	once(function () messages$windows_sequential())
} )()

check_paropts <- function (paropts) {
	"check that "

	arg_names <- names(paropts)		
	valid_formals <- names(formals(parallel::mclapply))
	invalid_formals <- setdiff(arg_names, valid_formals)
	
	(length(invalid_formals) > 0) %throws% 
		messages$invalid_paropts(func_call, invalid_formals)
	
	paropts$FUN <- NULL
	paropts$X <- NULL
	
	remove(invalid_formals, valid_formals, arg_names)
}

call_mclapply <- function (f, xs, paropts = NULL, func_call = "call_mclapply(f, xs, paropts)") {
	"(a -> b) -> [a] -> [b]
	a wrapper that maps f over x in parallel, and 
	 returns the results. OS-specific implementation."

	require_a("unary function", f, func_call)
	require_a("listy", xs, func_call)
	require_a(c("named list", "named pairlist"), paropts, func_call)

	if (.Platform$OS.type == 'windows') {
		warn_windows_users()
		lapply(xs, f)
	} else {

		if (length(paropts) > 0) {
			check_paropts(paropts)
		} else {
			cores_option <- getOption("mc.cores")
			paropts <- list(
				mc.cores = if (is.null(cores_option)) {
					1
				} else {
					cores_option
				}
			)
		}

		if (paropts$mc.cores == 1) {
			lapply(xs, f)
		} else {
			
			status <- ""
			output <- withCallingHandlers({	
				do.call(
					what = parallel::mclapply,
					args = c(list(FUN = f, X = xs), paropts))
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
}




