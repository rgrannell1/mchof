
# tools for manipulating and comparing formal arguments
# # # 

formal_names_equal <- function (f, g) {
	# are formals names of f equal to the formal names of g?
	
	formals <- list(
		f = names(formals(f)), g = names(formals(g)) )
	
	if (length(formals$f) != length(formals$g)) return (FALSE)
	
	all(formals$f == formals$g)
}

formal_defaults_equal <- function (f, g) {
	# are formals names of f equal to the formal names of g?
	
	formals <- list(
		f = unname(formals(f)), g = unname(formals(g)))
	
	identical(formals$f, formals$g)
}

formals_equal <- function (f, g) {
	# are the formal names and values identical?
	
	formals <- list(f = formals(f), g = formals(g))
	
	if (length(names(formals$f)) != length(names(formals$g))) {
		return (FALSE)
	}	
	all( names(formals$f) == names(formals$g) ) &&
	identical(unname(formals$f), unname(formals$g))
}

arg_names_equal <- function (f, g) {
	# are the argument names of f the same as those of g?

	arg_names <- list(
		f = names( head(as.list(args(f)), -1) ),
		g = names( head(as.list(args(g)), -1) )
	)

	if (length(arg_names$f) != length(arg_names$g)) {
		return (FALSE)
	}

	all(arg_names$f == arg_names$g)
}

arg_defaults_equal <- function (f, g) {
	# are the default values of f and g identical?

	arg_defaults <- list(
		f = unname( head(as.list(args(f)), -1) ),
		g = unname( head(as.list(args(g)), -1) )
	)

	identical(arg_defaults$f, arg_defaults$g)

}

is_ellipses <- function (formals) {
	identical(formals, formals( function (...) NULL ))
}

empty_formals <- function (names) {
	# construct an empty alist with names as names
	
	empty_symbol <- list(formals(function (x){ })$x)

	structure(
		replicate(n = length(names), expr = empty_symbol),
		names = names)
}

combine_formals <- function (func, f, g = f, envir = parent.frame()) {
	# try to add the most useful formals to 
	# func possible, by preserving the formals of f and g
	
	formals_func <- if (is.primitive(f) || is.primitive(g)) {

		if (arg_names_equal(f, g)) {
			if (arg_defaults_equal(f, g)) {
				head( as.list(args(f)), -1 )
			} else {
				empty_formals( head( as.list(args(g)), -1) )
			}
		}

	} else {
		if (formal_names_equal(f, g)) {
			if (formal_defaults_equal(f, g)) formals(f) else {
				empty_formals( names(formals(f)) )
			}
		}
	}	
	
	str_func <- gsub(
		pattern = "params_", 
		replacement = paste0(names(formals_func), collapse = ", "),
		x = paste0(deparse(func), collapse = "\n"))

	func <- eval(parse(text = str_func)) # create the function

	formals(func) <- formals_func
	environment(func) <- envir
	func
}

