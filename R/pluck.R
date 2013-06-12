#'
#' @title mcPluck
#' 
#' @description mcPluck iterates over a list x, and extract slots matching name
#' in each sublist. If x is a vector then keys matching name are returned.
#'  
#' @export
#' @param name a name or regular expression for keys to be returned.
#' @param x a list or vector.
#' @param paropts a list of parameters to be handed to 
#'    mclapply (see \link{mchof}).
#'    
#' @details mcPluck can operate on lists in which not every sublist contains 
#' a named slot matching pattern. If multiple matches are found in a sublist they
#' are all returned.
#'    
#' @example inst/examples/examples-pluck.r 
#' @keywords mcPluck

mcPluck <- function (pattern, x, paropts = NULL) {
	# extract entries matching pattern
	
	ISSUE("pluck not done!")
	
	func_call <- "mcPluck(pattern, x, paropts = NULL)"
	
	missing(pattern) %throws% messages$string_is_required(func_call, "pattern")
	missing(x) %throws% messages$vector_is_required(func_call, "x")
	(length(pattern) > 1) %throws% messages$not_string(func_call, "pattern")
	
	if (is.null(x)) return (NULL)
	if (length(x) == 0) return (x)
	is.factor(x) %throws% messages$was_factor(func_call, "x")

	select_name <- function (elem) {
		# vectors and lists!

		id <- if (is.list(elem)) list() else elem[0]
		
		if (length(names(vect)) == 0) return (id)		
			
		unname(vect[ grepl(pattern, names(vect)) ])
	}
	
	if (is.list(x)) {

	} else {
		if (length(names(x)) == 0) return (x[0])
		
		unlist(call_mclapply(
			function (chunk) {
				# map over chunk select_name
			},
			chop_into(x, get_cores(paropts)),
			paropts, func_call))
	}
}














