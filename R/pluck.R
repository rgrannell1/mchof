#'
#' @title mcPluck
#' 
#' @description mcPluck
#'  
#' @export
#' @param name a name or regular expression for keys to be returned.
#' @param x a list or vector.
#' @param paropts a list of parameters to be handed to 
#'    mclapply (see \link{mchof}).
#'    
#' @details 
#'    
#' @example inst/examples/examples-pluck.r 
#' @keywords mcPluck

mcPluck <- function (pattern, x, paropts = NULL) {
	# iterate over x, extract x$name where possible
	
	func_call <- "mcFilter(f, x, paropts = NULL)"
	
	missing(name) %throws% messages$string_is_required(func_call, "name")
	missing(x) %throws% messages$vector_is_required(func_call, "g")
	(length(name) > 1) %throws% messages$not_string(func_call, "name")
	
	if (is.null(x)) return (NULL)
	if (length(x) == 0) return (x)
	is.factor(x) %throws% messages$was_factor(func_call, "x")

	f <- function (elem) {

	}
	
	mcFilter(
		f = mcNot(is.null),
		call_mclapply(f, x, paropts, func_call))
}