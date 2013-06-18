
parameters <- parametres <- function (f) {
	# get the formals (closure) or arguments
	# (primitive) of f
	
	missing(f) %throws% messages$function_is_required(func_call, "f")

	if (is.primitive(f)) {
		head( as.list(args(f)), -1)
	} else {
		formals(f)
	}
}

"parameters<-" <- "parametres<-" <- function (f, x) {
	# setter to complement parameters
	
	func_call <- "parameters(f) <- x"

	missing(f) %throws% messages$function_is_required(func_call, "f")
	missing(x) %throws% messages$vector_is_required(func_call, "x")

	is.factor(x) %throws% messages$was_a_factor(func_call, x, "x")
	is.primitive(f) %throws% messages$cant_set_parameters(func_call, f, "f")

	if (is.list(x)) {

		(any_unnamed(x)) %throws% 
			messages$not_all_named(func_call, x, "x")
	
		formals(f) <- x
		f
	} else {

		nothing <- list( formals(function (x) { })$x )

		formals(f) <- structure(
			replicate(length(x), nothing),
			names = x)
		f
	}
}