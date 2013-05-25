#' @description mcZipWith takes n lists or vectors, generates a list of n element lists,
#' and returns the result of mapping f over this new list. 
#'  
#' @title mcZipWith
#' 
#' @export
#' 
#' @param f a function that takes a single n-element list, or a string
#' giving the name of such a function.
#' @param x a list or vector.
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
#' @seealso see \code{\link{mclapply}} for more details about the parallel
#'     backend being employed, \code{\link{mcZip}} for a varient of this function
#'     and \code{\link{mcUnzipWith}} for the inverse of this function.
#' @keywords mcZipWith
#' @example inst/examples/examples-zipwith.r

mcZipWith <- function (f, x, paropts = NULL) {
	# takes n lists/vectors, generates a list of n-tuples. 
	# returns the result of mapping f over this new list. 
	# excess elements are discarded. 
	
	# list (x1, x2), list (y1, y2)  |-> 
	# list ( list(x1, y1), list(x2, y2) )
	
	func_call <- deparse(match.call())
	
	f <- match.fun(f)
	if (is.null(x)) return (NULL)
	if (is.list(x) && length(x) == 0) return (list())

	lists <- Filter(
		function (elem) {

			inherits(elem, 'factor') %throws% stopf (
				'%s x must be a list of vectors or lists; actual value was %s (%s)', 
				func_call, deparse(elem), class(elem))
			
			!is.null(elem)
		}, x)

	shortest <- min(sapply (lists, length))

	if (shortest == 0) return(list())

	to_zip <- Map (
		function (el) el[seq_len(shortest)], 
		lists) 

	zipped <- call_mclapply (
		f = function (ind) {
			lapply (to_zip, function (x) x[[ind]])
		},	
		x = seq_len(shortest), 
		paropts )
	
	call_mclapply (
		f = f,	
		x = zipped,
		paropts )
}

#' @description mcZip takes n lists/vectors, and generates a list of n element lists.
#' It is a special case of mcZipWith
#' 
#' @title mcZip
#' @author Ryan Grannell
#' 
#' @export
#' @param x a list of lists
#' @param paropts a list of parameters to be handed to 
#' mclapply (see \link{mchof}).
#' 
#' @return returns a list of n element lists.
#'    
#' @details list names are dropped without warning during zipping; an example below shows how to add
#' names to the output list. NULL elements in x are automatically removed from x. The empty list is not 
#' removed in order act as a 'zero' to preserve useful structural identities.
#' 
#' the input lists are assumed to be of equal length; if they are not excess elements are discarded
#' without warning.
#' 
#' @seealso see \code{\link{mcUnzip}} for the inverse of 
#'     this function, and \code{\link{mcZipWith}} for a more general version 
#'     of this function.
#' 
#' @keywords mcZip
#' @example inst/examples/examples-zip.r

mcZip <- function(x, paropts = NULL) {
	# special case of mcZipWith: applies identity to result
	
	mcZipWith (f = identity, x, paropts = paropts)

}
