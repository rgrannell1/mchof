
#' Higher-Order-Functions for Functional Logic
#'
#' @description
#' \code{mcAnd} takes two functions \code{f} and \code{g}, and returns a composite function. This composite 
#' function returns \code{f(...) && g(...)}.
#'
#' \code{mcNot} takes a function \code{f}, and returns a function. This function returns the logical
#' negation of \code{f}: if \code{f} returns \code{TRUE} for a value \code{x} then \code{mcNot(f)} will
#' return \code{FALSE} (and visa versa when \code{f(x) == FALSE}). 
#'
#' \code{mcOr} takes two functions \code{f} and \code{g}, and returns a composite function. This composite 
#' function returns  \code{f(...) || g(...)}.
#'
#' \code{mcXor} takes two functions \code{f} and \code{g}, and returns a composite function. This composite 
#' function returns \code{xor( f(...), g(...) )}. The \code{xor} function returns
#' \code{TRUE} if either its first or second argument is \code{true}, but not both and not neither.
#'
#' @details
#'
#' All of these functions return logical values (\code{TRUE}, \code{FALSE}, or \code{NA}) and not boolean values
#' (\code{TRUE}, \code{FALSE}). \code{NA} values will be obtained if either \code{f} or \code{g} returns \code{NA}
#' when called with a particular value. This is easily altered by composing the output of \code{mcAnd}, \code{mcNot}, ...
#' with\code{\link{mcBoolean}}. For example,
#'
#' \code{finite_number = mcBoolean \%of\% mcAnd(is.numeric, is.finite)}
#'
#' \code{finite_number} first checks whether its input is numeric and finite, and then coerces this result to \code{TRUE} or \code{FALSE}.
#'
#' The composite functions formals depend on the input function(s):
#'
#' \enumerate{
#'     \item If the function \code{f} and \code{g} have the same parameter names - in the same order -
#'     but have different default arguments, then the output function will
#'     preserve only the parameters of \code{f}/\code{g}.
#'     \item If the parameters of \code{f} and \code{g} differ, or their order is shuffled then
#'     the output function uses ellipses as parameters.
#'     \item primitive functions (such as \code{max}) are supported by mchof, and their arguments are processed
#' in a similar way to those of closures (standard R functions). 
#'}
#' For mcNot the parameters of \code{f} are automatically copied to the output composite function.
#' @rdname mchof_logic
#' @keywords mcAnd mcNot mcOr mcXor
#'
#' @param f a function that returns a logical value, or a string giving the name of such a function.
#' @param g a function that returns a logical value, or a string giving the name of such a function.
#'
#' @family mchof-logic
#' @example inst/examples/examples-logic.r
#' @export

mcAnd <- function (f, g) {
	# return a function that returns true
	# if (f(...) && g(...))

	binary_functional(f, g, "&&", "mcAnd(f, g)")

}

#' @rdname mchof_logic
#' @family mchof-logic
#' @export

mcNot <- function (f) {
	# return a function that returns false when f is true, 
	# true when f is false, na when na
	
	func_call <- "mcNot(f)"

	missing(f) %throws%  messages$function_is_required(func_call, "f")

	f <- match.fun(f)
	
	rm(func_call)
	
	combine_formals(
		function () {
			!f(params_)
		}, f)
}

#' @rdname mchof_logic
#' @family mchof-logic
#' @export

mcOr <- function (f, g) {
	# return a function that returns true
	# if (f(...) || g(...))
	
	binary_functional(f, g, "||", "mcOr(f, g)")

}

#' @rdname mchof_logic
#' @family mchof-logic
#' @export

mcXor <- function (f, g) {
	# return a function that returns true
	# if (f(...) xor g(...))
	
	func_call <- "mcXor(f, g)"

	binary_functional(f, g, "xor", "mcXor(f, g)")

}
