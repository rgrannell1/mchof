


#' @export

mcAutoCurry <- function (f) {
	# transform a function that takes many 
	# variables into a chain of single variable function
	# calls. Invoke when every parameter has
	# an explicit value or default.

}

mcAutoCurry(function (a, b, c) a + b + c ) -> aa


