#'
#' @title mcFlip
#' 
#' @export
#' @description mcFlip takes a function f, and returns f with its formal arguments
#' reversed.
#' 
#' @details mcFlip can operate on functions with ellipses (...) and normal parameters,
#' but it cannot currently operate on functions with primitive functions such as 
#' "+". mcFlip preserves the default arguments of f.
#'  
#' @name mcFlip
#' 
#' @param f a function with normal parameters and/or ellipses (...).
#'
#' @return returns f with its formal parameters reversed.
#' @example inst/examples/examples-flip.r

mcFlip <- function (f) {
	# return a function with reversed formal arguments
	
	func_call <- "mcFlip(f)"

	missing(f) %throws% messages$function_is_required(func_call, "f")

	f <- match.fun(f)
	if (length(formals(f)) < 2) return (f)

	formals(f) <- rev(formals(f))
	f
}

#'
#' @title mcJumble
#' 
#' @export
#' @description mcJumble takes a function f, and returns f with its formal arguments
#' reversed.
#' 
#' @details mcJumble can operate on functions with ellipses (...) and normal parameters,
#' but it cannot currently operate on functions with primitive functions such as 
#' "+". mcJumble preserves the default arguments of f.
#' 
#' the vector x determines how the formals of f will be permuted. For example,
#' if \code{x = c(3,1,2), f = function (a, b, c) sum(a, b, c)} the new formals 
#' are c, a, b; the 3rd element in formals, the 1st element in formals and the 
#' 2nd element in formals.
#'  
#' @name mcJumble
#' 
#' @param f a function with normal parameters and/or ellipses (...).
#' @param a vector x that is the same length as the formal parameters of x and contains
#' all the numbers 1, 2, ..., length(formals). This vector determines how the 
#' formals of f will be permuted (see details).
#'
#' @return returns f with its formal parameters permuted by x.
#' @example inst/examples/examples-jumble.r

mcJumble <- function (f, x) {
	# returns a function with its formals rearrange, 
	# according to a index vector (bijection) 
	
	func_call <- "mcJumble(f, x)"
	
	missing(f) %throws% messages$function_is_required(func_call, "f")
	missing(x) %throws% messages$vector_is_required(func_call, "x")

	f <- match.fun(f)
	if (length(formals(f)) < 2) return (f)
	
	( !all(x %in% seq_along(formals(f))) ) %throws% 
		messages$must_be_indices(func_call, x, "x")
	
	(length(formals(f)) != length(x)) %throws% 
		messages$length_mismatch(
			func_call, c(length(formals(f)),
			length(x)), "formals(f)", "x")
	
	duplicates <- unique(names(formals(f))[ x[duplicated(x)] ])
	
	(length(unique(x)) != length(x)) %throws% 
		messages$matched_multiple_time(
			func_call, paste0(duplicates, collapse = ", "), "x")
		
	formals(f) <- formals(f)[c(x)]
	f
}

