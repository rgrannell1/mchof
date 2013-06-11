
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
	# return a function that returns false when f is true, 
	# true when f is false, na when na
	
	func_call <- paste0( deparse(match.call()), ':' )

	missing(f) %throws% stopf (
		'%s a function (or function name) f is required but was missing',
		func_call)
	
	f <- match.fun(f)
	function (...) {
		Sys.sleep(time = n)
		f(...)
	}
}
