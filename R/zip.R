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
#' @details Names are dropped without warning during zipping; 
#' named outputs are given in the example below. 
#' mcZipWith discards excess elements without warning: for example 
#' \code{list (1, 2), list (3, 4, 5)} becomes 
#' \code{list (f( list(1, 3) ), f( list(2, 4) ))}. 
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
		function (el) {

			inherits(el, 'factor') %throws% stopf (
				'%s x must be a list of vectors or lists; actual value was %s (%s)', 
				func_call,
				deparse(el),
				class(el))

			!is.null(el) && any(c('list', 'vector') %in% is(el))

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
#' @details mcZip discards excess elements without warning: for example 
#' list (1, 2), list (3, 4, 5) becomes list (list(1, 3), list(2, 4)). 
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
