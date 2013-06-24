
messages <- list()

messages <- c(messages, list(
	class_mismatch = function (call, data, name, expected) {
		call <- head(call, 1)
		name <- head(name, 1)

		class_data <- paste0(class(data), collapse = ", ")
		
		stopf (
			'%s: %s had the wrong class (actual was %s, expected %s)',
			call, name, class_data, expected)	
	},
	value_required = function (call, spec, name) {
		call <- head(call, 1)
		name <- head(name, 1)

		stopf (
			'%s: a %s %s is required but was missing',
			call, spec, name)
	},
	function_is_required = function (call, name) {
		messages$value_required(call, "function (or function name)", name)
	},
	vector_is_required = function (call, name) {
		messages$value_required(call, "vector or list", name)
	},
	list_is_required = function (call, name) {
		messages$value_required(call, "list", name)
	},
	string_is_required = function (call, name) {
		messages$value_required(call, "string", name)
	}
))

messages <- c(messages, list(
	formals_has_ellipses = function (call, data, name) {
		call <- head(call, 1)
		name <- head(name, 1)
		
		stopf (
			"%s: ellipses (...) cannot be used in %s's formals",
			call, name)
	},
	cant_set_parameters = function (call, data, name) {
		call <- head(call, 1)
		name <- head(name, 1)
		
		stopf(
			"%s: cannot set %s's parameters, as it is a primitive function",
			call, name)

	},
	cant_be_parameters = function (call, data, name) {
		call <- head(call, 1)
		name <- head(name, 1)
		
		stopf(
			"%s: %s must be either a character vector or a list of parameter = default pairs",
			call, name)
	}
))

messages <- c(messages, list(
	invalid_paropts = function (call, data) {
		call <- head(call, 1)
		
		stopf(
			'%s: invalid arguments given to paropts (%s)', 
				 call, paste(data, collapse = ', '))
	},
	windows_sequential = function () {
		
		msg <- sample(
			c(paste0("parallel execution is not supported on windows: ",
				"executing on one core"),
			paste0("parallel execution is not supported on windows:",
				"executing on one core :/")),
			prob = c(0.7, 0.3), size = 1)
		
		warning (msg, call. = FALSE)
	}
))

messages <- c(messages, list(
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
	not_in = function (call, data, name_one, name_two) {
		call <- head(call, 1)
		name_one <- head(name_one, 1)
		name_two <- head(name_two, 1)

		stopf (
			'%s: %s not in %s',
			call, name_one, name_two)	
	}
))


