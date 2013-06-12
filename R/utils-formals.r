
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

empty_formals <- function (names) {
	# construct an empty alist with names as names
	
	empty_symbol <- list(formals(function (x){ })$x)

	structure(
		replicate(n = length(names), expr = empty_symbol),
		names = names)
}

set_formals <- function (func, f, g) {
	# try to set the formals of func to 
	# those of f and g, if they have similar formals.
	# returns ellipses for primitive functions
	
	if (is.primitive(f) || is.primitive(g)) {
		return (func)
	}
	
	if (formal_names_equal(f, g)) {
		if (formal_defaults_equal(f, g)) {
			formals(func) <- formals(f)
		} else {
			formals(func) <- empty_formals( names(formals(f)) )
		}
	}
	func
}
