
#' Higher-Order-Functions for Functional Arithmetic
#'
#' @description
#'
#'\code{mcLarger} takes two functions \code{f} and \code{g}, and returns a composite function.
#' This composite function returns \code{f(...) > g(...)}
#'
#' \code{mcSmaller}, \code{mcPlus}, \code{mcMinus}, \code{mcEqual}, \code{mcNotEqual}, \code{mcMultiply}, \code{mcDivide},  \code{ \code{mcMax} and \code{mcMin} are equivalent wrappers for their respective operators.
#'
#' @details 
#'
#' No input validation is done for the resulting function returned by functions in this family. If incorrect inputs are given then standard R error 
#' checking is performed, but there are no special checks in place to check the type or length of the arguments/outputs. This is to ensure that the 
#' wrappers in this family allow the underlying function to behave identically to their base equivelants. 
#'
#' This family of functions is particularily useful for manipulating continuous mathematical functions such as lines, polynomials and periodic waves.Functions like \code{mcPlus} will output a vectorised function if both of its input functions are vectorised. For example,
#'
#' \code{ f = mcPlus(function (x) x^2, function (x) 40*sin(x)) }
#' 
#' \code{ f(1:3) }
#'
#' \code{ [1]  34.65884  40.37190  14.64480 }
#'
#' the output of \code{f} is vectorised because both input functions are vectorised. Both the input functions and the output function are shown below.
#'
#' \figure{math-example.png}
#'
#' @param \code{f} a function that returns an appropriate value (a number for every function but \code{mcEqual} and \code{mcNotEqual}), or a string giving the name of such a function.
#' @param g a function with the same restrictions as \code{f}
#'
#' @rdname mchof_math
#' @family mchof-math
#' @example inst/examples/examples-math.r
#'
#' @keywords mcLarger mcSmaller mcPlus mcMinus mcEqual mcNotEqual mcMultiply mcDivide mcMax mcMin
#'
#' @export

mcLarger <- function (f, g) {
	# (a -> numeric) -> (a -> numeric) -> (a -> boolean)

	binary_functional(f, g, ">", "mcLarger(f, g)")

}

#' @rdname mchof_math
#' @family mchof-math
#' @export

mcSmaller <- function (f, g) {
	# (a -> numeric) -> (a -> numeric) -> (a -> boolean)

	binary_functional(f, g, "<", "mcSmaller(f, g)")

}

#' @rdname mchof_math
#' @family mchof-math
#' @export

mcPlus <- function (f, g) {
	# (a -> numeric) -> (a -> numeric) -> (a -> numeric)

	binary_functional(f, g, "+", "mcPlus(f, g)")
}

#' @rdname mchof_math
#' @family mchof-math
#' @export

mcMinus <- function (f, g) {
	# (a -> numeric) -> (a -> numeric) -> (a -> numeric)

	binary_functional(f, g, "-", "mcMinus(f, g)")

}

#' @rdname mchof_math
#' @family mchof-math
#' @export

mcEqual <- function (f, g) {
	# (a -> numeric) -> (a -> numeric) -> (a -> boolean)

	binary_functional(f, g, "==", "mcEqual(f, g)")

}

#' @rdname mchof_math
#' @family mchof-math mchof-logic
#' @export

mcNotEqual <- function (f, g) {
	# (a -> numeric) -> (a -> numeric) -> (a -> boolean)

	binary_functional(f, g, "!=", "mcNotEqual(f, g)")

}

#' @rdname mchof_math
#' @family mchof-math mchof-logic
#' @export

mcMultiply <- function (f, g) {
	# (a -> numeric) -> (a -> numeric) -> (a -> numeric)

	binary_functional(f, g, "*", "mcMultiply(f, g)")

}

#' @rdname mchof_math
#' @family mchof-math
#' @export

mcDivide <- function (f, g) {
	# (a -> numeric) -> (a -> numeric) -> (a -> numeric)

	binary_functional(f, g, "/", "mcDivide(f, g)")

}

#' @rdname mchof_math
#' @family mchof-math
#' @export

mcMax <- function (f, g) {
	# (a -> numeric) -> (a -> numeric) -> (a -> numeric)

	binary_functional(f, g, "max", "mcMax(f, g)")

}

#' @rdname mchof_math
#' @family mchof-math
#' @export

mcMin <- function (f, g) {
	# (a -> numeric) -> (a -> numeric) -> (a -> numeric)

	binary_functional(f, g, "min", "mcMin(f, g)")

}