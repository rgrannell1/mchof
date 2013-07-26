#'
#' Fold-like Higher-Order-Functions
#'
#' @description
#' \code{mcFold} applies an associative binary function \code{f} to a list or vector \code{x},
#' returning a single value. If x is length zero then an initial value is returned.
#'
#' \code{mcReduce} applies an associative binary function \code{f} to a list or vector \code{x},
#' returning a single value. 
#'
#' @param f a binary function that takes two of "a thing" and returns one of a "thing" (see details).
#' @param x a list or vector.
#' @param z an initial value to be prepended to \code{x}
#' @param paropts a list of parameters to be handed to 
#'    mclapply (see \link{mchof}).
#'
#' @details \code{mcFold} and \code{mcReduce} function can be used as a parallel alternative to the non-parallel folding functions if
#' and only if the function \code{f} is associative; that is
#'
#' \deqn{(a f b) f c = a f (b f c)}
#' 
#' where \eqn{a, b, c} are values that \code{f} takes. For example, plus is an associative 
#' binary operator, since
#'
#' \deqn{(a + b) + c = a + (b + c)}
#'
#' for any number \eqn{a, b, c}. Minus does not have this property, so it is not 
#' suitable for use with \code{mcFold}. Only associative binary functions can be folded 
#' or reduced in parallel. 
#' 
#' Formally the combination of an associative binary operator,
#' an identity element for that operator and a set (x) is known as a monoid; the function \code{f}
#' has a type signature of \eqn{[A] -> [A] -> [A]}. A likely source of errors when using \code{mcFold}
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
#' @export

mcFold <- function (f, x, xs, paropts = NULL) {
	"(a -> b -> a) -> a -> [b] -> a
	 swaps the commas in z, x1, x2, ..., xn with
	 the function f."

	pcall <- sys.call()
	require_a("functionable", f, pcall)
	require_a("any", x, pcall)
	require_a("listy", xs, pcall)
	require_a(c("named list", "named pairlist"), paropts, pcall)

	f <- match.fun(f)
	require_a('binary function', f, pcall)
 
	if (length(xs) == 0) {
		x
	} else {
		mcReduce(f, xs = c(list(x), xs), paropts)		
	}
}

#' @rdname mchof_folds
#' @family mchof-folds
#' @export

mcReduce <- function (f, xs, paropts = NULL) {
	"(a -> b -> a) -> [a] -> a
	 swaps the commas in x1, x2, x3, ..., xn with
	 the infix function f."

	iterateWhile <- function (f, p, x) {
		# pipe the output x of f into f, 
		# until p(x) is true
		
		while( !p(x) ) x <- f(x)
		x
	} 
	
	pcall <- sys.call()
	
	require_a("functionable", f, pcall)
	require_a("listy", xs, pcall)
	require_a(c("named list", "named pairlist"), paropts, pcall)

	f <- match.fun(f)
	
	require_a("binary function", f, pcall)
	if (length(xs) < 2) {
		xs
	} else {
		g <- function (xs) {
			if (length(xs) == 2) {
				f( xs[[1]], xs[[2]] )
			} else {
				xs[[1]]
			}	
		}

		final <- iterateWhile(
			function (reducable) {
				group_into(call_mclapply(g, reducable, paropts, pcall), n = 2)
			},
			function (reducable) {
				length(reducable) == 1
			},
			group_into(xs, n = 2))
		
		g( final[[1]] )		
	}
}
