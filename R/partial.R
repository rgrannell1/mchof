
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
	# take f and fill in some of its arguments
	
	func_call <- "mcPartial(f, ...)"
	
	missing(f) %throws% messages$function_is_required(func_call, "f")
	
	f <- match.fun(f)	
	added <- list(...)
	
	rm(func_call)
	
	function (...) {
		do.call( f, c(added, list(...)) )
  	}
}

#' @description mcPartialf ttransforms a function that takes multiple arguments
#' into a function that takes less arguments by partially applying the function with
#' the arguments supplied.This function differs from mcPartial in that it 
#' alters the formals of its return function rather than using ellipses (...).
#'
#' @title mcPartialf
#'  
#' @export
#' @param f a function with no variadic (...) or primitive arguments,
#' or a string giving the name of such a function.
#' @param ... name = value pairs to apply to f.
#' @return returns a partially applied version of f, with modified formals.
#' 
#' @details mcPartialf is superficially similar to mcPartial, but it differs in the kind
#' of function that it returns. mcPartialf modifies the formals parameters of f, 
#' rather than just setting the formals of f to ellipses (...). This allows other
#' functionals to modify the formals of an mcPartialf'd function, but not one 
#' modified by mcPartial.
#' 
#' The downside to this implementation is that functions with variadic 
#' arguments (...) or primitive arguments (base function such as plus) do not
#' work well with mcPartialf.
#' 
#' @seealso see \code{\link{mcPartial}} for a partial application function that 
#' works with variadic and primitive formal parameters.
#' 
#' @keywords mcPartialf
#' @example inst/examples/examples-partialf.r

mcPartialf <- function (f, ...) {
	# take f and fill in some of its arguments, return a function 
	# with less formals
	
	func_call <- "mcPartialf(f, ...)"

	missing(f) %throws% function_is_required(func_call, "f")
	
	f <- match.fun(f)
	added <- list(...)
	
	formals_f <- names(formals(f))
	duplicated_names <- paste0(
		names(added)[ duplicated(names(added)) ],
		collapse = ", ")
	
	("..." %in% formals_f) %throws% message$formals_has_ellipses(
		func_call, formals_f, "f")

	(length( which(names(added) != "") ) < length(added)) %throws% 
		message$not_all_named(func_call, "...")
	
	(length(unique(names(added))) != length(added)) %throws% 
		message$matched_muliple_times(func_call, duplicated_names, "...")
	
	rm(formals_f, func_call, duplicated_names)
	
	g <- function () {
		do.call(
			what = f,
			args = c(added, as.list(match.call())[-1]) )
	}
	
	formals_f <- names(formals(f))
	formals_g <- formals_f[ !formals_f %in% names(added) ]
	empty_symbol <- list(formals(function (x){ })$x)
	
	formals(g) <- structure(
		replicate(n = length(formals_g), expr = empty_symbol),
		names = formals_g)
	g
}


