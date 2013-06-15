
# tools for manipulating and comparing formal arguments
# # # 

formal_names_equal <- function (f, g) {
	# are formals names of f equal to the formal names of g?
	
	formals <- list(
		f = names(formals(f)),
		g = names(formals(g)) )
	
	if (length(formals$f) != length(formals$g)) return (FALSE)
	
	all(formals$f == formals$g)
}

formal_defaults_equal <- function (f, g) {
	# are formals names of f equal to the formal names of g?
	
	formals <- list(
		f = unname(formals(f)),
		g = unname(formals(g)))
	
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

insert_params <- function (formals, func, envir = parent.frame()) {
	# don't judge, eval parse is much less
	# error prone than fiddling with envirnoments. simple is best.
	# internal inputs are guaranteed to be synactically correct, 
	# so there isn't much to worry about.

	str_func <- gsub(
		pattern = "params", 
		replacement = paste0(names(formals), collapse = ", "),
		x = deparse(func))

	func <- eval(parse(text = str_func))
	formals(func) <- formals
	environment(func) <- envir
	func
}

combine_formals <- function (func, f, g = f, envir = parent.frame()) {
	# try to add the most useful formals to 
	# func possible, by preserving the formals of f and g
	
	formals_func <- if (is.primitive(f) || is.primitive(g)) {
		formals( function (...) NULL )
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


















