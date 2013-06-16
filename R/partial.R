
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
#' @seealso see \code{\link{mcPartialf}} for a partial application function that 
#' operates on the formals of f and performs input validation.
#' 
#' @keywords mcPartial
#' @example inst/examples/examples-partial.r

mcPartial <- function (f, ...) {
	# take f and fill in some of its arguments, return a function 
	# with less formals
	
	func_call <- "mcPartialf(f, ...)"

	missing(f) %throws% messages$function_is_required(func_call, "f")
	
	f <- match.fun(f)
	added <- list(...)
	
	formals_f <- names(formals(f))
	duplicated_names <- paste0(
		names(added)[ duplicated(names(added)) ],
		collapse = ", ")
	
	("..." %in% formals_f) %throws% messages$formals_has_ellipses(
		func_call, formals_f, "f")

	(length( which(names(added) != "") ) < length(added)) %throws% 
		messages$not_all_named(func_call, added, "...")
	
	(length(unique(names(added))) != length(added)) %throws% 
		messages$matched_muliple_times(func_call, duplicated_names, "...")
	
	rm(formals_f, func_call, duplicated_names)
	
	g <- function () {
		do.call(
			what = f,
			args = c(added, as.list(match.call())[-1]) )
	}
	
	formals_f <- names(formals(f))
	formal_names_g <- formals_f[ !formals_f %in% names(added) ]
	
	formals(g) <- empty_formals(formal_names_g)
	g
}
