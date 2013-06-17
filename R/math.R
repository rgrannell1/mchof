
#' Higher-Order-Functions for Functional Arithmetic
#'
#' @description
#'
#'\code{mcLarger} takes two functions \code{f} and \code{g}, and returns a composite function.
#' this composite function returns \code{f(...) > g(...)}
#'
#'\code{mcSmaller} takes two functions \code{f} and \code{g}, and returns a composite function.
#' this composite function returns \code{f(...) < g(...)}
#''
#'\code{mcPlus} takes two functions \code{f} and \code{g}, and returns a composite function.
#' this composite function returns \code{f(...) + g(...)}
#'
#'\code{mcMinus} takes two functions \code{f} and \code{g}, and returns a composite function.
#' this composite function returns \code{f(...) - g(...)}
#'
#'\code{mcEqual} takes two functions \code{f} and \code{g}, and returns a composite function.
#' this composite function returns \code{f(...) == g(...)}
#'
#'\code{mcNotEqual} takes two functions \code{f} and \code{g}, and returns a composite function.
#' this composite function returns \code{f(...) != g(...)}
#'
#'\code{mcMultiply} takes two functions \code{f} and \code{g}, and returns a composite function.
#' this composite function returns \code{f(...) * g(...)}
#'
#'\code{mcDivide} takes two functions \code{f} and \code{g}, and returns a composite function.
#' this composite function returns \code{f(...) / g(...)}
#'
#' @details 1
#'
#' @param f a function that returns a value, or a string giving the name of such a function.
#' @param g a function that returns a value, or a string giving the name of such a function.
#'
#' @rdname mchof_math
#' @family mchof-math
#' @example inst/examples/examples-math.r
#' @export


mcLarger <- function (f, g) {

	binary_functional(f, g, ">", "mcLarger(f, g)")

}

#' @rdname mchof_math
#' @family mchof-math
#' @export

mcSmaller <- function (f, g) {

	binary_functional(f, g, "<", "mcSmaller(f, g)")

}

#' @rdname mchof_math
#' @family mchof-math
#' @export

mcPlus <- function (f, g) {

	binary_functional(f, g, "+", "mcPlus(f, g)")
}

#' @rdname mchof_math
#' @family mchof-math
#' @export

mcMinus <- function (f, g) {

	binary_functional(f, g, "-", "mcMinus(f, g)")

}

#' @rdname mchof_math
#' @family mchof-math
#' @export

mcEqual <- function (f, g) {
	# returns a function that returns f(...) == g(...)

	binary_functional(f, g, "==", "mcEqual(f, g)")

}

#' @rdname mchof_math
#' @family mchof-math mchof-logic
#' @export

mcNotEqual <- function (f, g) {
	# returns a function that returns f(...) == g(...)

	binary_functional(f, g, "!=", "mcNotEqual(f, g)")

}

#' @rdname mchof_math
#' @family mchof-math mchof-logic
#' @export

mcMultiply <- function (f, g) {
	# returns a function that returns f(...) == g(...)

	binary_functional(f, g, "*", "mcMultiply(f, g)")

}

#' @rdname mchof_math
#' @family mchof-math
#' @export

mcDivide <- function (f, g) {
	# returns a function that returns f(...) == g(...)

	binary_functional(f, g, "/", "mcDivide(f, g)")

}
