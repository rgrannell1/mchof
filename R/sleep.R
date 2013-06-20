
#' Functions for Controlling Temporal Flow of a Program
#'
#' @description 1
#'
#' @details 1
#'
#' @param \code{f} a function or a string giving the name of a function.
#' @param \code{n} a number that is interperated as seconds
#'
#' @rdname mchof_sleep
#' @family mchof-sleep
#' @example inst/examples/examples-sleep.r
#' @export

mcSleep <- function (f, n) {
	# side-effectful function
	# sleep for n seconds before calling f. Less
	# accurate on non-UNIX-like OS's
	
	n <- abs(n)
	func_call <- "mcSleep(f, n)"

	missing(f) %throws% messages$function_is_required(func_call, "f")
	(!is.numeric(n)) %throws% messages$not_a_number(func_call, n, "n")
	
	f <- match.fun(f)	
	
	combine_formals(
		function (...) {
			Sys.sleep(time = n)
			f(params_)
		}, f)
}

#' @rdname mchof_sleep
#' @family mchof-sleep
#' @export

mcTimer <- function (n) {
	# int -> ( -> bool)
	# returns a function with Sys.time( ) 
	# captured in a closure
	
	n <- abs(n)
	func_call <- "mcTimer(n)"

	(!is.numeric(n)) %throws% 
		messages$not_a_number(func_call, n, "n")
	
	( function () {
		start_time <- Sys.time()
		function () {
			time_passed <- as.numeric(difftime( Sys.time(), start_time ))
			time_passed < n
		}		
	} )()
}

