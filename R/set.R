
#' Higher-Order-Functions for Set Theory
#' 
#' @description 1
#'
#' @details 1
#'
#' @param f a function, or a string giving the name of a function.
#' @param g a function, or a string giving the name of a function.
#'
#' @rdname mchof_set
#' @family mchof-set
#' @example inst/examples/examples-set.r
#' @export

mcUnion <- function (f, g) {

	binary_combinator(f, g, "union", "mcUnion(f, g)")

}

#' @rdname mchof_set
#' @family mchof-set
#' @export

mcIntersect <- function (f, g) {

	binary_combinator(f, g, "intersect", "mcUnion(f, g)")
}

#' @rdname mchof_set
#' @family mchof-set
#' @export

mcSetDiff <- function (f, g) {

	binary_combinator(f, g, "setdiff", "mcSetDiff(f, g)")

}

#' @rdname mchof_set
#' @family mchof-set
#' @export

mcSymDiff <- function (f, g) {

	symdiff <- function (a, b) {
		union( setdiff(a, b), setdiff(b, a) )
	}

	binary_combinator(f, g, "symdiff", "mcSetDiff(f, g)")

}

#' @rdname mchof_set
#' @family mchof-set
#' @export

mcSetEqual <- function (f, g) {

	binary_combinator(f, g, "setequal", "mcSetEqual(f, g)")

}

#' @rdname mchof_set
#' @family mchof-set
#' @export

mcIsElement <- function (f, g) {

	binary_combinator(f, g, "is.element", "mcIsElement(f, g)")

}

#' @rdname mchof_set
#' @family mchof-set
#' @export

mcSymDiff <- function (f, g) {

	ISSUE("Fix symdiff")
	symdiff <- function (a, b) {
		union( setdiff(a, b), setdiff(b, a) )
	}

	binary_combinator(f, g, "symdiff", "mcSetDiff(f, g)")

}

