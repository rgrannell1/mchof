
messages <- list(
	not_a_function = function (call, data, name) {
		
		stopf (
			"%s: %s is not a function (actual class was %s)",
			call, name, paste0(class(data), collapse = ", "))
	},
	function_is_required = function (call, name) {
		
		stopf (
			'%s: a function (or function name) %s is required but was missing',
			call, name)
	},
	formals_has_ellipses = function (call, data, name) {
		
		stopf (
			"%s: ellipses (...) cannot be used in %s's formals: actual formals were %s",
			call, name, paste0(data, collapse = ", "))
	},
	not_a_vector = function (call, data, name) {
		# warn that input isn't a vector
		
		stopf (
			'%s: %s is not a vector: actual class was %s',
			call, name, paste0(class(data), collapse = ", "))

	},
	vector_is_required = function (call, name) {
		# stop, a vector/list is needed
		
		stopf (
			'%s: a list or vector %s is required but was missing',
			call, name)
	},
	string_is_required = function (call, name) {
		
		stopf (
			'%s: a string %s is required but was missing',
			call, name)	
	},
	not_string = function (call, data, name) {
		stopf(
			'%s: %s was not a length-one character vector (length: %s, class: %s)',
			call, name, length(data), paste0(class(data), collapse = ", ")
		)
	},
	length_mismatch = function (call, data, name_one, name_two) {
		
		description <- paste(
			name_one, "had length", length( data[[1]] ), ",",
			name_two, "had length", length( data[[2]] ))
		
		stopf (
			'%s: length mismatch between %s and %s (%s)',
			call, name_one, name_two, description)	
	},
	not_all_named = function (call, data, name) {
		
		which_not_named <- paste0(
			which(names(data) == ""), collapse = ", ")
		
		stopf (
			"%s: not every argument in %s was named (%s)",
			call, name, which_not_named)
	},
	matched_multiple_time = function (call, data, name) {
	
		duplicates <- unique(names(data)[ data[duplicated(data)] ])
		duplicates <- paste0(duplicates, collapse = ", ")
		stopf(
			"%s: some arguments in %s were duplicated (%s)",
			call, name, duplicates)
	},
	was_factor = function (call, data, name) {
		
		stopf (
			'%s: a list or vector %s was expected but a factor was given',
			call, name)
	},
	these_were_factors = function (call, which, name) {
		
		stopf (
			"%s: elements %s in %s were factors",
			call, which, name)
	},
	not_a_bool = function (call, data, name) {
		
		stopf (
			"%s: %s wasn't a TRUE or FALSE value",
			call, name)
	},
	not_a_number = function (call, data, name) {
		stopf (
			"%s: %s wasn't a number",
			call, name)
	},
	windows_sequential = function () {
		# warn windows users that parallel isn't possible
		
		msg <- sample(
			c(paste0("parallel execution is not supported on windows: ",
				"\n", "executing on one core"),
			paste0("parallel execution is not supported on windows:",
				"\n", "executing on one core :/")),
			prob = c(0.7, 0.3), size = 1)
		
		warning (msg, call. = FALSE)
	},
	invalid_paropts = function (call, data) {
		# stop, paropts had incorrect options
		
		stopf(
			'invalid arguments given to paropts: %s', 
				 paste(invalid_args, collapse = ', '))
	}
)


