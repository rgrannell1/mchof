
mcFlip <- function (f) {
	# return a function with reversed formal arguments
	
	func_call <- "mcFlip(f)"

	missing(f) %throws% messages$function_is_required(func_call, "f")

	f <- match.fun(f)
	if (length(formals(f)) == 0) return (f)

	formals(f) <- rev(formals(f))
	f
}

mcJumble <- function (f, x) {
	# returns a function with its formals rearrange, 
	# according to a index vector (bijection) 
	
	func_call <- "mcJumble(f, x)"
	
	missing(f) %throws% messages$function_is_required(func_call, "f")
	missing(x) %throws% messages$vector_is_required(func_call, "x")

	f <- match.fun(f)
	if (length(formals(f)) == 0) return (f)
	
	( !all(x %in% seq_along(formals(f))) ) %throws% 
		messages$must_be_indices(func_call, x, "x")
	
	(length(formals(f)) != length(x)) %throws% 
		messages$length_mismatch(
			func_call, c(length(formals(f)),
			length(x)), "formals(f)", "x")
	
	duplicates <- unique(names(formals(f))[ x[duplicated(x)] ])
	
	(length(unique(x)) != length(x)) %throws% 
		messages$matched_multiple_time(
			func_call, paste0(duplicates, collapse = ", "), "x")
		
	formals(f) <- formals(f)[c(x)]
	f
}

