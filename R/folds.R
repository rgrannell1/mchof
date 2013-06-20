#'
#' Fold-like Higher-Order-Functions
#'
#' @description
#' \code{mcFold} applies an associative binary function \code{f} to a list or vector \code{x},
#' returning a single value. If x is length zero then an initial value is returned.
#'
#' \code{mcFold} applies an associative binary function \code{f} to a list or vector \code{x},
#' returning a single value. 
#'
#' @param f a binary function that takes two of "a thing" and returns one of a "thing".
#' @param x a list or vector.
#' @param z an initial value to be prepended to x
#' @param paropts a list of parameters to be handed to 
#'    mclapply (see \link{mchof}).
#'
#' @details this function can be used as a parallel alternative to foldl or reducel if
#' and only if the function f is associative; that is
#'
#' \code{(a f b) f c == a f (b f c)}, 
#' 
#' where a, b or c are values that f takes. For example, plus is an associative 
#' binary operator, since
#'
#' \code{(a + b) + c == a + (b + c)}
#'
#' for any number a, b or c. Minus does not have this property, so it is not 
#' suitable for use with mcFold. Only associative binary functions can be folded 
#' or reduced in parallel. 
#' 
#' Formally the combination of an associative binary operator,
#' an identity element for that operator and a set (x) is known as a monoid; the function \code{f}
#' has a type signature of [A] -> [A] -> [A]. A likely source of errors when using \code{mcFold}
#' or \code{mcReduce} is using a function without this type signature (ie. a function that
#' takes two of a thing, and returns one of a thing).
#' 
#' with \code{mcFold} it useful to use the identity of \code{f} as first, as it can make it 
#' possible to simplify \code{f}. For example, lists have an identity element of \code{list()}
#' when concatenated, and integers have an identity of 0 under addition. 
#' 
#' @section Special Cases:
#'
#' when x is NULL, NULL is automatically returned (since NULL falls throught all mchof functions without 
#' being interperated as meaningful data). If x is a length-zero value such as list() or integer(0) then
#' \code{mcFold} returns \code{first}, and \code{mcReduce} automatically returns \code{x}. 
#'
#' \code{mcReduce} returnes length-one values because a binary function cannot be applied 
#' to a single value, so the value is presumed to be already fully "reduced".
#'
#' @example inst/examples/examples-folds.r
#' @rdname mchof_folds
#' @family mchof-folds
#' @example inst/examples/examples-folds.r
#' @export

mcFold <- function (f, z, x, paropts = NULL) {
	# (a -> b -> a) -> a -> [b] -> a
	# swaps the commas in z, x1, x2, ..., xn with
	# the function f.
		
	func_call <- "mcFold(f, z, x, paropts = NULL)"

	missing(f) %throws% messages$function_is_required(func_call, "f")
	missing(z) %throws% messages$vector_is_required(func_call, "z")
	missing(x) %throws% messages$vector_is_required(func_call, "x")
 
	if (is.null(x)) return (NULL)
	if (length(x) == 0) return (z)
	
	is.factor(x) %throws% messages$was_factor(func_call, x, "x")
	
	mcReduce(f, x = c(list(z), x), paropts)
	
}

#' @rdname mchof_folds
#' @family mchof-folds
#' @export

mcReduce <- function (f, x, paropts = NULL) {
	# (a -> b -> a) -> [a] -> a
	# swaps the commas in x1, x2, x3, ..., xn with
	# the infix function f.

	iterateWhile <- function (f, p, x) {
		# pipe the output x of f into f, 
		# until p(x) is true
		
		while( !p(x) ) x <- f(x)
		x
	} 
	
	func_call <- "mcReduce(f, x, paropts = NULL)"

	missing(f) %throws% messages$function_is_required(func_call, "f")
	missing(x) %throws% messages$vector_is_required(func_call, "x")
		
	f <- match.fun(f)
	if (length(x) < 2) return (x)
	is.factor(x) %throws% messages$was_factor(func_call, x, "x")
	
	g <- function (x) {
		if (length(x) == 2) f( x[[1]], x[[2]] ) else x[[1]]	
	}

	final <- iterateWhile(
		function (reducable) {
			group_into(call_mclapply(g, reducable, paropts, func_call), size = 2)
		},
		function (reducable) {
			length(reducable) == 1
		},
		group_into(x, size = 2))
	
	g( final[[1]] )
}

#' @rdname mchof_folds
#' @family mchof-folds
#' @export

mcFoldl <- function (f, z, x) {
	# (a -> b -> a) -> a -> [b] -> a
	# fold a list, starting from the left
	
	func_call <- "mcFoldl(f, z, x)"

	missing(f) %throws% messages$function_is_required(func_call, "f")
	missing(x) %throws% messages$vector_is_required(func_call, "x")
		
	f <- match.fun(f)
	if (length(x) < 2) return (x)
	is.factor(x) %throws% messages$was_factor(func_call, x, "x")
	
	if (is.null(x)) return (NULL)
	if (length(x) == 0) return (z)

	ind <- 1
	len_x <- length(x)

    while (ind <= len_x) {
    	z <- f( x[[ind]], z )
    	ind <- ind + 1
    }
    z
}

#' @rdname mchof_folds
#' @family mchof-folds
#' @export

mcFoldr <- function (f, z, x) {
	# (a -> b -> b) -> b -> [a] -> b
	# fold a list, starting from the right
	
	func_call <- "mcFoldr(f, z, x)"

	missing(f) %throws% messages$function_is_required(func_call, "f")
	missing(x) %throws% messages$vector_is_required(func_call, "x")
		
	f <- match.fun(f)
	if (length(x) < 2) return (x)
	is.factor(x) %throws% messages$was_factor(func_call, x, "x")
	
	if (is.null(x)) return (NULL)
	if (length(x) == 0) return (z)

	ind <- len_x <- length(x)

    while (ind > 0) {
    	z <- f( x[[ind]], z )
    	ind <- ind - 1
    }
    z
}

#' @rdname mchof_folds
#' @family mchof-folds
#' @export

mcReducel <- function (f, x) {
	# (a -> b -> a) -> [b] -> a
	# fold a list, starting from the left
	
	ind <- len_x <- length(x)

    while (ind > 0) {
    	z <- f( x[[ind]], z )
    	ind <- ind - 1
    }
    z
}

#' @rdname mchof_folds
#' @family mchof-folds
#' @export

mcReducer <- function (f, x) {
	# (a -> b -> a) -> [b] -> a
	# fold a list, starting from the left
	
	len_x <- length(x)
	ind <- length(x) - 1
	z <- head(x, -1)

    while (ind > 0) {
    	z <- f( x[[ind]], z )
    	ind <- ind - 1
    }
    z
}
