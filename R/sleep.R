
#' @description mcSleep takes a function f, and returns a function that waits
#' n seconds before calling f
#'
#' @title mcSleep
#' 
#' @export
#' @param f a function or a string giving the name of 
#' a function.
#' @param n the amount of time to sleep before executing f, in seconds
#' @return returns a function that calls f after n seconds
#' 
#' @details mcSleep is useful for stalling quick functions that execute 
#' queries to a website with a rate limit, 
#' to avoid hitting the rate limit before it refreshes.
#'  
#' @keywords mcSleep
#' @example inst/examples/examples-sleep.r

mcSleep <- function (f, n) {
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

mcTimer <- function (n) {
	# returns a function with Sys.time( ) 
	# captured in a closure
	
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

