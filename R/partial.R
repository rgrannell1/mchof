
#' @description mcPartial transforms a function that takes multiple arguments
#' into a function that takes less arguments by partially applying the function with
#' the arguments supplied.
#'
#' @title mcPartial
#'  
#' @export
#' @param f a function, or a string giving the name of a function.
#' @param ... name = value pairs to apply to f.
#' @return returns a partially applied function with ellipsis (...) formals
#' 
#' @details mcPartial returns a partially applied function; an example of a partially
#' applied function is 
#' 
#' f(x,y) 2x + y -> f(x) 2x + 2 
#' 
#' @keywords mcPartial
#' @example inst/examples/examples-partial.r

mcPartial <- function (f, ...) {
	# take f and fill in some of its arguments, return a function 
	# with less formals
	
	func_call <- "mcPartial(f, ...)"

	missing(f) %throws% messages$function_is_required(func_call, "f")
	
	ISSUE("partial is very broken")
	
	f <- match.fun(f)
	applied <- list(...)
	
	formals_f <- names(formals(f))
	
	("..." %in% formals_f) %throws% 
		messages$formals_has_ellipses(func_call, formals_f, "f")
	
	( any_unnamed(names(applied)) ) %throws% 
		messages$not_all_named(func_call, applied, "...")
	
	( any_duplicated(names(applied)) ) %throws% 
		messages$matched_muliple_times(func_call, applied, "...")

	rm(formals_f, func_call)

	applied_func <- function () {

		do.call(f, c(applied, as.list(match.call())[-1]) )
	}
	
	formals_f <- names(formals(f))
	remaining_params <- formals_f[ !formals_f %in% names(applied) ]
	
	formals(applied_func) <- empty_formals(remaining_params)
	applied_func
}
