
#' @export

mcAutoPartial <- function (f) {
	# transform a function that takes many 
	# variables into a chain of function
	# calls. Invoke when every parameter has
	# an explicit value or default.

	# returns a wrapper function for f (args = args(f))
		# if f has no parameters left, invoke it!
		# if f has parameters left, 

		mcParameters(
			function () {
				"this function takes arguments,"
				"partially applies them,"
				"and returns a partially applying function of smaller arity"
				""
				this <- list(
					func = sys.function(sys.parent()),
					args = mcArguments()
				)
				this$params <- mcParameters(this$func)

				p <- mcPartial(
					f = this$func,
					x = this$args
				)

				if (length(mcParameters(p)) == 0) {
					p()
				} else {
					mcAutoPartial(p)
				}
			},
			mcParameters(f))
}

