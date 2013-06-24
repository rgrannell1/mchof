
#' Functions for Modifying Function Parameters
#'
#' @description
#'
#'\code{mcFlip} mcFlip takes a function \code{f}, and returns \code{f} with its parameters
#' reversed.
#'
#'\code{mcJumble} takes a function \code{f}, and returns \code{f} with its parameters
#' permuted as defined by \code{x}.
#'
#'\code{mcParameters} gets the parameters of a function if it is called with just a function \code{f},
#'and returns a function with its parameters set to \code{x} if called with a function \code{f} and a list \code{x}. If it is called with a function \code{f} and character vector \code{x} then the parameters of \code{f} are set to \code{x}, with no default arguments.
#'
#'\code{mcExplode} takes a single variable function and returns an adapted function that takes a variable number of arguments before passing them to its underlying function.
#'
#'\code{mcImplode} takes a variadic function and returns an adapted function that takes a single list of arguments, and passes its argument to its underlying function.
#'
#' @details 
#'
#' the vector x determines how the formals of f will be permuted. For example,
#' if \code{x = c(3, 1, 2), f = function (a, b, c) sum(a, b, c)} the new formals 
#' are c, a, b; the 3rd element in formals, the 1st element in formals and the 
#' 2nd element in formals.
#'
#' @param f a function, or a string giving the name of a function.
#' @param x a vector that is the same length as the formal parameters of x and contains
#' all the integers 1, 2, ..., length(arguments). This vector determines how the 
#' arguments of f will be permuted.

#' @rdname mchof_parameters
#' @family mchof-parameters
#' @example inst/examples/examples-parameters.r
#' @export

mcFlip <- function (f) {
	# (a -> b -> ... -> z) -> (z -> ... -> b -> a)
	# return a function with reversed formal arguments
	
	func_call <- "mcFlip(f)"

	missing(f) %throws% 
		messages$function_is_required(func_call, "f")

	f <- match.fun(f)
	
	if (length(mcParameters(f)) < 2) f else {
		mcParameters(f, rev(mcParameters(f)))
	}
}

#' @rdname mchof_parameters
#' @family mchof-parameters
#' @export

mcJumble <- function (f, x) {
	# function -> function 
	# returns a function with its formals rearranged, 
	# according to a index vector (bijection) 
	
	func_call <- "mcJumble(f, x)"
	
	missing(f) %throws% 
		messages$function_is_required(func_call, "f")
	missing(x) %throws% 
		messages$vector_is_required(func_call, "x")

	(!is.vector(x)) %throws% 
		messages$class_mismatch(func_call, x, "x", "vector or list")
	
	f <- match.fun(f)
	if (length(mcParameters(f)) < 2) return (f)
	
	( !all(x %in% seq_along(mcParameters(f))) ) %throws% 
		messages$must_be_indices(func_call, x, "x")
	
	(length(mcParameters(f)) != length(x)) %throws% 
		messages$length_mismatch(
			func_call, list(mcParameters(f), x), "mcParameters(f)", "x")

	(any_duplicated(x)) %throws% 
		messages$matched_multiple_time(func_call, x, "x")

	mcParameters(
		f,
		mcParameters(f)[c(x)])
}

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

#' @rdname mchof_parameters
#' @family mchof-parameters
#' @export

mcArguments <- function () {
	# return a list of arguments that the function 
	# in which this function is called in...is called with

	func_call <- "mcArguments()"
	
	defaults <- mcParameters( sys.function(sys.parent()) )
	required <- as.list( match.call(
		def = sys.function( -1 ),
		call = sys.call(-1)) )[-1]

	structure(
		 Map(
			function (default, param) {
				has_no_default <- identical(
					default,
					formals( function (x) {} )$x)
				
				if (has_no_default) {
					required[[param]]
				} else {
					default
				}
			},
			unname(defaults),
			names(defaults)
		),
		names = names(defaults)
	)
}

#' @rdname mchof_parameters
#' @family mchof-parameters
#' @export

mcParameters <- function (f, x) {
	# (a -> b -> ... -> z) -> [a, b, ..., z]
	# (a -> b -> ... -> z) -> [x1, x2, ..., xn] -> (x1 -> x2 -> ... -> xn)
	# get the formals/arguments of f if x
	# isn't given, and set the formals if x is given.
	# the formal environment of f is set to parent.frame(),
	# since no relevant variables are being added here

	func_call <- "mcParameters(f, x)"

	missing(f) %throws% 
		messages$function_is_required(func_call, "f")

	f <- match.fun(f)

	use_as_getter <- missing(x)

	if (use_as_getter) return (
		if (is.primitive(f)) {
			head( as.list(args(f)), -1 )
		} else {
			formals(f)
		}
	)

	is_correct_class <- 
		(is.vector(x) && is.character(x)) || is.list(x) 

	(!is_correct_class) %throws%
		messages$class_mismatch(func_call, x, "x", "character vector or list")
 
	if (is.list(x)) {
		# set the parameters to x

		(any_unnamed(x)) %throws% 
			messages$not_all_named(func_call, x, "x")
	
		if (is.primitive(f)) {
			# wrap the primitive before adding parameters

			g <- function () {
				'a closure wrapping a primitive function'
				''

				.args <- structure(
					Map(
						function (name) {
							# given a name get its value binding 
							# with normal scoping rules

							eval(as.symbol(name))
						},
						names(formals())),
						name = names(formals())
					)

				do.call(f, .args)
			}
			formals(g) <- x
			return (g)
		}

		formals(f) <- x
		return (f)
	}

 	if (is.vector(x) && is.character(x)) {
		# if x is a character vector then 
		# set the parameters to the names theirin

		any_duplicated(x) %throws%
			messages$matched_multiple_time(func_call, x, "x")

		missing_default <- list( formals(function (x) { })$x )

		if (length(x) > 0) {
			if (is.primitive(f)) {
				# wrap the primitive before adding parameter names

				g <- function () {
					'a closure wrapping a primitive function'
					''
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

				formals(g) <- structure(
					replicate(length(x), missing_default),
					names = x)
				return (g)
			}

			formals(f) <- structure(
				replicate(length(x), missing_default),
				names = x)
			return (f)

		} else {

			if (is.primitive(f)) {
				# wrap primitive before adding no parameters

				g <- function () {
					'a closure wrapping a primitive function'
					''
					f()
				}
				return (g)
			}

			formals(f) <- list()
			return (f)
		}
	}
}

#' @rdname mchof_parameters
#' @family mchof-parameters
#' @export

mcExplode <- function (f) {
	# (a -> b) -> (... -> b)
	# takes a function that takes a single value and 
	# makes it into a variadic function

	func_call <- "mcExplode(f)"

	missing(f) %throws% 
		messages$function_is_required(func_call, "f")
	f <- match.fun(f)

	function (...) {
		'an exploded function; takes a variable number'
		'of arguments, puts them in a list and calls its'
		'underlying function with that singe list'

		f(list(...))
	}
}

#' @rdname mchof_parameters
#' @family mchof-parameters
#' @export

mcImplode <- function (f) {
	# (... -> b) -> (a -> b)
	# dual to mcExplode. 
	# takes a function that takes a many values and 
	# makes it into a function that takes one list

	func_call <- "mcImplode(f)"

	missing(f) %throws% 
		messages$function_is_required(func_call, "f")
	f <- match.fun(f)

	function (x) {
		'an imploded function; takes a single argument x and'
		'calls f with x[[1]]...x[[n]]'

		do.call(f, c(list(), x))
	}
}

ISSUE("fix partial; variables aren't being bound properly")

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

	mcParameters(
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

	mcParameters(
		function () {
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
		mcParameters(f))
}
