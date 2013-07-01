
xParams <- function (f) {
# get the formals of non-primitive functions, and
# the arguments of primitive functions

	require_a(c('function', 'string'), f)
	
	f <- match.fun(f)
	
	if (is.primitive(f)) {
	head( as.list(args(f)), -1 )
	} else {
	formals(f)
	}
}

require_a <- function (properties, value) {
	# checks that a value has a set of properties.
	# properties is a character vector,
	# 	where each element is a space seperated list of properties that
	# 	must all be true for the property to be true.
	# at least one element of properties must be true, or an 
	#	error is thrown 
	
	input <- list(
		name = paste0(
			deparse( as.list( match.call() )[-1]$value),
			 collapse = ''),
		call = paste0(
			deparse( sys.call(sys.parent()) ),
			collapse = '')
	)

	force(value)
	force(properties)

	properties <- unname(Map(
		function (group) {
			unlist(strsplit(group, '[ 	]+'))
		},
		properties
	))

	properties_description <- paste0(
		sapply(properties, function (group) {
			paste0(group, collapse = ' and ')
		}),
		collapse = ', or '
	)

	has_property <- function (value, property) {
		# does value have some property? these are usually
		# class or magnitude related

		property_test <- switch(property,
			'any' =
				function (value) TRUE,
			'binary' = 
				function (value) {

					if (!is.function(value)) return (FALSE)
					params <- xParams(value)

					if ('...' %in% names(params)) TRUE else {
						length(params) == 2
					}
				},
			'boolean' = 
				function (value) {
					is.logical(value) && !is.na(value)
				},
			'character' = 
				function (value) {
					is.character(value)
				},
			'finite' = 
				is.finite,
			'function' = 
				is.function,				
			'list' = 
				is.list,
			'logical' = 
				is.logical,
			'string' =
				function (value) {
					is.character(value) && length(value) == 1
				},
			'named' = 
				function (value) {

					if (length(value) > 0) {
						!is.null(value) && all(nchar(names(value)) > 0)
					} else TRUE
				},
			'nonnegative' = 
				function (value) {
					is.numeric(value) >= 0
				},
			'null' = 
				is.null,
			'numeric' =
				is.numeric,
			'pairlist' = 
				is.pairlist,
			'positive' = 
				function (value) {
					is.numeric(value) && value > 0
				},
			'unary' = 
				function (value) {

					if (!is.function(value)) return (FALSE)
					params <- xParams(value)

					if ('...' %in% names(params)) TRUE else {
						length(params) == 1
					}
				},	
			'vector' = 
				is.vector,
			'whole' = 
				function (value) {
					abs(round(value) - value) < .Machine$double.eps
				}
			)
	
		tryCatch({
				res <- property_test(value)

				is.na(res) %throws%
					stopf('NA was returned when testing whether %s had property %s',
						input$name, property)
				res
			},
			warning = function (warn) {

				warningf(c(
					'',
					'\t\ta warning was encounted while testing %s for property %s',
					'\t\tthe class of the input %s was %s',
					'\t\tthe test being applied to the input was:',
					'',
					'\t\t%s',
					'',
					'\t\tthe warning was %s'),
					input$name, property,
					input$name, paste0(class(value), collapse = ', '),
					deparse(property_test),
					warn$message
				)

			},
			error = function (err) {

				stopf(c(
					'',
					'\t\tan error was encounted while testing %s for property %s',
					'\t\tthe class of the input %s was %s',
					'\t\tthe test being applied to the input was:',
					'',
					'\t\t%s',
					'',
					'\t\tthe error was %s'),
					input$name, property,
					input$name, paste0(class(value), collapse = ', '),
					deparse(property_test),
					err$message
				)

		})
	}

	ulapply <- function (x, f) unlist(lapply(x, f))

	had_property <- ulapply(
		properties,
		function (group) {
			all( ulapply(group, function (member) {
				has_property(value, member)
			}) )
		}
	)

	(!any(had_property)) %throws%
		messages$property_mismatch(
			input$call,
			value,
			input$name,
			properties_description
		)
}
