
registrar <- list(
	add_function = function (func) {

		registrar$functions <<- c(registrar$functions, func)
	},
	add_property = function (func, property, family = "mchof") {
		# add a property to registrar

		registrar$properties <<- c(
			registrar$properties,
			list(
				func = func,
				property = property)
		)
	},
	all_have_property = function (property, family, except = c()) {
		# do all functions (or just family members)
		# have the property?

		mcAll(
			function (members) {
				# check each function
			},
			mcSelect(
				function (record) {

					record$family %in% family &&
					!(record$func %in% except)
				},
				registrar$properties
			)
		)

	},
	which_have_property = function (property, family) {
		# return the names of functions with a given property

		sapply(
			mcSelect(
				function (tuple) {
					(tuple$properties == property) &&
					(tuple$func %in% family)
				},
				registrar$properties
			), 
			function (tuple) {
				tuple$func
			}
		)

	},
	get_properties = function (func) {
		# get the properties of a single function

		(!(func %in% registrar$functions)) %throws% 
			messages$not_in(
				"registrar$get_properties()",
				 list(func, registrar$functions),
				"func", "registrar$functions")

		sapply(
			mcSelect(
				function (record) {
					record$func == func
				},
				registrar$properties
			),
			function (record) {
				record$properties
			}			
		)

	},
	functions = c(),
	properties = list()
)