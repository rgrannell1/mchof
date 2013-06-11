
messages <- list(
	not_a_function <- function (call, f) {
		# warn that f isn't a function
		
		stopf (
			"%s: f is not a function: actual value was %s (%s)",
			call,
			paste0(deparse(f), collapse = "\n"),
			paste0(class(f), collapse = ', '))
	},
	function_is_required <- function (call, name) {
		stopf (
			'%s: a function (or function name) %s is required but was missing',
			call, name)
	},
	formals_has_ellipses <- function (call, formals, name) {
		stopf(
			"%s: ellipses (...) cannot be used in %s's formals: actual formals were %s",
			func_call, formals, paste0(formals_f, collapse = ", "))
	},
	not_a_vector <- function (call, x) {
		# warn that input isn't a vector
		
		stopf (
			'%s: x is not a vector: actual value was %s (%s)',
			call,
			paste0(deparse(x), collapse = "\n"),
			paste0(class(x), collapse = ', '))
	},
	windows_sequential <- function () {
		# warn windows users that parallel isn't possible
		
		msg <- sample(
			c(paste0("parallel execution is not supported on windows: ",
				"\n", "executing on one core"),
			paste0("parallel execution is not supported on windows:",
				"\n", "executing on one core :/")),
			prob = c(0.8, 0.2), size = 1)
		
		warning (msg, call. = FALSE)
	},
	invalid_paropts <- function (invalid_args) {
		stopf(
			'invalid arguments given to paropts: %s', 
				 paste(invalid_args, collapse = ', '))
	}
)





