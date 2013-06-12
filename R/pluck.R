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
	
	func_call <- "mcPluck(pattern, x, paropts = NULL)"
	
	missing(name) %throws% messages$string_is_required(func_call, "name")
	missing(x) %throws% messages$vector_is_required(func_call, "g")
	(length(name) > 1) %throws% messages$not_string(func_call, "name")
	
	if (is.null(x)) return (NULL)
	if (length(x) == 0) return (x)
	is.factor(x) %throws% messages$was_factor(func_call, "x")

	select_name <- function (elem) {
		# vectors and lists!
	}
	
	if (is.list(x)) {
		mcFilter(
			f = mcNot(is.null),
			call_mclapply(select_name, x, paropts, func_call))
	} else {
		unlist(call_mclapply(
			function (chunk) {
	
				unname(chuck[ grepl(pattern, names(chunk)) ])
			},
			chop_into(x, get_cores(paropts)),
			paropts, func_call))
	}
}














