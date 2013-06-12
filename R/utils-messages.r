
messages <- list(
	not_a_function = function (call, data, name) {
		call <- head(call, 1)
		name <- head(name, 1)
		
		stopf (
			"%s: %s is not a function (actual class was %s)",
			call, name, paste0(class(data), collapse = ", "))
	},
	function_is_required = function (call, name) {
		call <- head(call, 1)
		name <- head(name, 1)

		stopf (
			'%s: a function (or function name) %s is required but was missing',
			call, name)
	},
	formals_has_ellipses = function (call, data, name) {
		call <- head(call, 1)
		name <- head(name, 1)
		
		stopf (
			"%s: ellipses (...) cannot be used in %s's formals",
			call, name)
	},
	not_a_vector = function (call, data, name) {
		call <- head(call, 1)
		name <- head(name, 1)
		
		stopf (
			'%s: %s is not a vector (actual class was %s)',
			call, name, paste0(class(data), collapse = ", "))

	},
	vector_is_required = function (call, name) {
		call <- head(call, 1)
		name <- head(name, 1)
		
		stopf (
			'%s: a list or vector %s is required but was missing',
			call, name)
	},
	string_is_required = function (call, name) {
		call <- head(call, 1)
		name <- head(name, 1)
		
		stopf (
			'%s: a string %s is required but was missing',
			call, name)	
	},
	not_string = function (call, data, name) {
		call <- head(call, 1)
		name <- head(name, 1)
		
		stopf(
			'%s: %s was not a length-one character vector (length: %s, class: %s)',
			call, name, length(data), paste0(class(data), collapse = ", ")
		)
	},
	length_mismatch = function (call, data, name_one, name_two) {
		call <- head(call, 1)
		name_one <- head(name_one, 1)
		name_two <- head(name_two, 1)

		description <- if (is.vector(data) && length(data) > 1) {
			paste(
				name_one, "had length", length( data[[1]] ), ",",
				name_two, "had length", length( data[[2]] ))
		} else ""
		
		stopf (
			'%s: length mismatch between %s and %s (%s)',
			call, name_one, name_two, description)	
	},
	not_all_named = function (call, data, name) {
		call <- head(call, 1)
		name <- head(name, 1)
		
		which_not_named <- if (!is.vector(data)) {
			paste0(which(names(data) == ""), collapse = ", ")
		} else ""
				
		stopf (
			"%s: not every argument in %s was named (%s)",
			call, name, which_not_named)
	},
	matched_multiple_time = function (call, data, name) {
		call <- head(call, 1)
		name <- head(name, 1)
		
		duplicates <- if (is.vector(data)) {
			paste0(
				unique(names(data)[ data[duplicated(data)] ]),
				collapse = ", ")
		} else ""

		stopf(
			"%s: some arguments in %s were duplicated (%s)",
			call, name, duplicates)
	},
	was_factor = function (call, data, name) {
		call <- head(call, 1)
		name <- head(name, 1)
		
		stopf (
			'%s: a list or vector %s was expected but a factor was given',
			call, name)
	},
	these_were_factors = function (call, which, name) {
		call <- head(call, 1)
		name <- head(name, 1)
		
		stopf (
			"%s: elements %s in %s were factors",
			call, paste0(which, collapse = ", "), name)
	},
	not_a_bool = function (call, data, name) {
		call <- head(call, 1)
		name <- head(name, 1)
		
		stopf (
			"%s: %s wasn't a TRUE or FALSE value",
			call, name)
	},
	not_a_number = function (call, data, name) {
		call <- head(call, 1)
		name <- head(name, 1)

		stopf (
			"%s: %s wasn't a number (actual class was %s)",
			call, name, paste0(class(data), collapse = ", "))
	},
	windows_sequential = function () {
		
		msg <- sample(
			c(paste0("parallel execution is not supported on windows: ",
				"executing on one core"),
			paste0("parallel execution is not supported on windows:",
				"executing on one core :/")),
			prob = c(0.7, 0.3), size = 1)
		
		warning (msg, call. = FALSE)
	},
	invalid_paropts = function (call, data) {
		call <- head(call, 1)
		
		stopf(
			'invalid arguments given to paropts (%s)', 
				 paste(data, collapse = ', '))
	}
)


