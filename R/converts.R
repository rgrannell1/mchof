
#' export 

mcBoolean <- function (f, na_is_true = FALSE) {
	# (a -> b) -> (a -> boolean)
	# convert the output of f to a TRUE or FALSE value

	func_call <- "mcBoolean(f, na_is_true)"

	ISSUE("finish mcBoolean")

	missing(f) %throws% messages$function_is_required(func_call, "f")

	f <- match.fun(f)
	
	rm(func_call)

	if (na_is_true) {

		combine_formals(
			function () {	
				# converts the return value of f to boolean:
				# (TRUE -> TRUE), (FALSE, NA -> FALSE)

				res <- as.logical(f(params_))
				isTRUE(res)
			}, f)	
	} else {

		combine_formals(
			function () {
				# converts the return value of f to boolean:
				# (TRUE, NA -> TRUE), (FALSE -> FALSE)

				res <- as.logical(f(params_))
				isTRUE(res) || is.na(res)
			}, f)
	}
	
}