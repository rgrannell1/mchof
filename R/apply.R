
#' @rdname mchof_parameters
#' @family mchof-parameters
#' @export

mcApply <- function (f, x) {
	# call the function f with the list x. If
	# x is a character vector then look up the 
	# names in x and add them to a list.

	func_call <- "mcApply(f, x)"

	missing(f) %throws% messages$function_is_required(func_call, "f")
	missing(x) %throws% messages$vector_is_required(func_call, "x")

	(!is.vector(x)) %throws% 
		messages$class_mismatch(func_call, x, "x", "vector or list")

	f <- match.fun(f)
	.f_params <- mcParameters(f)

	if (is.list(x)) {

		if (!"..." %in% names(.f_params)) {
			# ensure f will work with x 

			(length(.f_params) != length(x)) %throws% 
				messages$length_mismatch(
					func_call, list(.f_params, x),
					"mcParameters(f)", "x")
		}

		do.call(f, x)

	} else if (is.character(x)) {
		
		if (!"..." %in% names(.f_params)) {
			# ensure f will work with x 

			(length(.f_params) != length(x)) %throws% 
				messages$length_mismatch(
					func_call, list(.f_params, x),
					"mcParameters(f)", "x")
		}

		.args <- structure(
			Map(
				function (name) {
					# given a name get its value binding 
					# with normal scoping rules

					eval(as.symbol(name))
				},
				x),
			name = x
		)

		do.call(f, .args)
	}
}

#' @rdname mchof_parameters
#' @family mchof-parameters
#' @export

"%apply%" <- mcApply

ISSUE("fix partial; lazy evaluation is totalling the code")

mcPartial <- function (f, x) {
	# takes a function f and binds some of its parameters
	# with values given in x, returning a function with smaller arity

	func_call <- "mcPartial(f, x)"

	missing(f) %throws% 
		messages$function_is_required(func_call, "f")
	missing(x) %throws%
		messages$list_is_required(func_call, "x")

	f <- match.fun(f)
	.formals_f <- names(mcParameters(f))

	(!is.list(x)) %throws% 
		messages$class_mismatch(func_call, x, "x", "list")

	("..." %in% .formals_f) %throws% 
		messages$formals_has_ellipses(func_call, .formals_f, "f")
	
	( any_unnamed(x) ) %throws% 
		messages$not_all_named(func_call, x, "x")
	
	( any_duplicated(names(x)) ) %throws% 
		messages$matched_muliple_times(func_call, x, "x")

	.fixed <- x
	rm(func_call, x)

	.remaining <- 
		mcParameters(f)[c(
			.formals_f[ !.formals_f %in% names(.fixed) ]
		)]

	mcReforge(
		function () {
			'a partially applied function'
			'(use environment(func)$.fixed to see fixed variables)'
			''
			.current <- Map(
				function(param) {
					eval(as.symbol(param))
				},
				names(formals())
			)
			do.call(f, c(.current, .fixed))

		}, .remaining)
}

mcAutoPartial <- function (f) {
	# transform a function that takes many variables into a chain of
	# functions. Invoke when every parameter has an explicit value or default.

	mcReforge(
		f = function () {
			"this function takes arguments,"
			"partially applies them to its underlying function,"
			"and returns a partially applying function of smaller arity"
			""
			this <- list(
				func = sys.function(sys.parent()),
				args = as.list( match.call() )[-1]
			)
			this$params <- mcParameters(this$func)

			p <- mcPartial(
				f = this$func,
				x = this$args
			)

			if (length(mcParameters(p)) == 0) {
				p()
			} else {
				all_have_defaults <- all(
				sapply(
				   	mcParameters(p),
				   	function (el) {
				   		!identical(el, formals(function (x){ })$x)
				   	})
				)

				if (all_have_defaults) {
					p()
				} else mcAutoPartial(p)
			}
		},
		x = mcParameters(f))
}

