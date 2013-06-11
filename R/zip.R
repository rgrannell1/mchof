#'   
#' @title mcZipWith
#' 
#' @description mcZipWith takes n lists or vectors, generates a list of n element lists,
#' and returns the result of calling f with each n element list. 
#' 
#' @export
#' 
#' @param f a function that takes n arguments, or a string
#' giving the name of such a function.
#' @param ... several lists or vectors.
#' @param paropts a list of parameters to be handed to 
#'    mclapply (see \link{mchof}).
#'    
#' @return returns the result of mapping f over a list of n element lists.
#'    
#' @details list names are dropped without warning during zipping; an example below shows how to add
#' names to the output list. NULL elements in x are automatically removed from x. The empty list is not 
#' removed in order act as a 'zero' to preserve useful structural identities.
#' 
#' the input lists are assumed to be of equal length; if they are not excess elements are discarded
#' without warning.
#' 
#' @seealso see \code{\link{mcZip}} for a varient of this function
#'     and \code{\link{mcUnzipWith}} for the inverse of this function.
#' @keywords mcZipWith
#' @example inst/examples/examples-zipwith.r

mcZipWith <- function (f, ..., paropts = NULL) {
	# takes n lists/vectors, generates a list of n-tuples. 
	# returns the result of mapping f over this new list. 
	# excess elements are discarded. 
	
	func_call <- deparse(match.call())
	
	missing(f) %throws% stopf (
		'%s a function (or function name) f is required but was missing',
		func_call)
	
	x <- list(...)
	f <- match.fun(f)

	if (is_list0(x)) return (list())
	if (is.null( x[[1]] )) return (NULL)
	
	sublist_info <- sapply(x, function (elem) {
		c(
			factor = inherits(elem, "factor"),
			not_null = !is.null(elem),
			length = length(elem))
	})
	
	any(sublist_info["factor",]) %throws% stopf(
		"%s elements %s were factors)",
		func_call,
		paste0(which(sublist_info["factor",]), collapse = ", "))

	min_length <- min(sublist_info["length",])
	
	if (min_length == 0) return (list())

	which_not_null <- which(sublist_info["not_null",] == 1)
	
	call_mclapply(
		function (ind) {
			# get the ind-th element in each sublist,
			# add to a list, apply f to that list
			
			do.call(f, lapply( x, function (sublist) sublist[[ind]] ))
		},
		seq_len(min_length),
		paropts
	)
}

#' @description mcZip takes n lists/vectors, and generates a list 
#' of n element lists. It is a special case of mcZipWith
#' 
#' @title mcZip
#' 
#' @export
#' @param ... several lists or vectors.
#' @param paropts a list of parameters to be handed to 
#'    mclapply (see \link{mchof}).
#' 
#' @return returns a list of n element lists.
#'    
#' @details list names are dropped without warning during zipping; an example below 
#' shows how to add
#' names to the output list. NULL elements in x are automatically removed from x.
#'  The empty list is not 
#' removed in order act as a 'zero' to preserve useful structural identities.
#' 
#' the input lists are assumed to be of equal length; if they are not excess 
#' elements are discarded without warning.
#' 
#' @seealso see \code{\link{mcUnzip}} for the inverse of 
#'     this function, and \code{\link{mcZipWith}} for a more general version 
#'     of this function.
#' 
#' @keywords mcZip
#' @example inst/examples/examples-zip.r

mcZip <- function(..., paropts = NULL) {
	# special case of mcZipWith: applies identity to result

	var_identity <- function (...) list(...)
	mcZipWith (f = var_identity, ..., paropts = paropts)

}
