


#' @export

mcAutoCurry <- function (f) {
	# transform a function that takes many 
	# variables into a chain of single variable function
	# calls. Invoke when every parameter has
	# an explicit value or default.

	mcParameters(
		function () {

			.current <- as.list( sys.call() )[-1]
			.formals <- formals()

			next_func <- function () {

				
				
			}
			mcParameters(
				next_func,
				tail(.formals, length(.current)))

		},
		mcParameters(f)
	)
}

#add <- mcAutoCurry(function (a, b, c) a + b + c)


