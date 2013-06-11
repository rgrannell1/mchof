
messages <- list(
	not_a_function = function (call, f) {
		# stop, f isn't a function
		
		stopf (
			"%s: f is not a function: actual value was %s (%s)",
			call,
			paste0(deparse(f), collapse = "\n"),
			paste0(class(f), collapse = ', '))
	},
	function_is_required = function (call, name) {
		# stop, a function/function string is needed
		
		stopf (
			'%s: a function (or function name) %s is required but was missing',
			call, name)
	},
	formals_has_ellipses = function (call, formals, name) {
		stopf(
			"%s: ellipses (...) cannot be used in %s's formals: actual formals were %s",
			func_call, formals, paste0(formals_f, collapse = ", "))
	},
	not_a_vector = function (call, x) {
		# warn that input isn't a vector
		
		stopf (
			'%s: x is not a vector: actual value was %s (%s)',
			call,
			paste0(deparse(x), collapse = "\n"),
			paste0(class(x), collapse = ', '))
	},
	vector_is_required = function (call, name) {
		# stop, a vector/list is needed
		
		stopf (
			'%s: a list or vector %s is required but was missing',
			call, name)
	},
	length_mismatch = function (call, length, name_one, name_two) {
		# stop, length one != length two
		
		stopf (
			'%s: length mismatch between %s and %s (%s)',
			call, length, name_one, name_two)	
	},
	not_all_named = function (call, name) {
		# stop, not all named
		
		stopf (
			"%s: not every argument in %s was named",
			call, name)
	},
	matched_multiple_time = function (call, duplicates, name) {
		stopf(
			"%s: some arguments in %s were duplicated (%s)",
			call, name, duplicates)
	},
	was_factor = function (call, name) {
		# stop, was a factor
		
		stopf (
			'%s: a list or vector %s but a factor was given',
			call, name)
	},
	these_were_factors = function (call, which, name) {
		# stop, these were factocts
		
		stopf (
			"%s: elements %s in %s were factors",
			call, which, name)
	},
	wasnt_boolean = function (call, name) {
		# stop, wasnt T/F
		
		stopf (
			"%s: %s wasn't a TRUE or FALSE value",
			call, name)
	},
	windows_sequential = function () {
		# warn windows users that parallel isn't possible
		
		msg <- sample(
			c(paste0("parallel execution is not supported on windows: ",
				"\n", "executing on one core"),
			paste0("parallel execution is not supported on windows:",
				"\n", "executing on one core :/")),
			prob = c(0.8, 0.2), size = 1)
		
		warning (msg, call. = FALSE)
	},
	invalid_paropts = function (invalid_args) {
		# stop, paropts had incorrect options
		
		stopf(
			'invalid arguments given to paropts: %s', 
				 paste(invalid_args, collapse = ', '))
	}
)


