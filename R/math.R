
#' Higher-Order-Functions for Functional Arithmetic
#'
#' @description 1
#'
#'
#'\code{mcLarger} takes two functions \code{f} and \code{g}, and returns a composite function.
#' this composite function returns \code{f(...) > g(...)}
#'
#' \code{mcSmaller}, \code{mcPlus}, \code{mcMinus}, \code{mcEqual}, \code{mcNotEqual}, \code{mcMultiply}, and \code{mcDivide} are equivalent wrappers for their respective operators.
#'
#' @details 1
#'
#' @param \code{f} a function that returns an appropriate value (a number for every function but \code{mcEqual} and \code{mcNotEqual}), or a string giving the name of such a function.
#' @param g a function with the same restrictions as \code{f}
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
