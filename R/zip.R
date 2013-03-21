
#' @description mcZipWith takes n lists/vectors, generates a list of n element lists,
#' and returns the result of mapping f over this new list. 
#' 
#' @title mcZipWith
#' @author Ryan Grannell
#' 
#' @export
#' @param f a function that takes a single n-element list
#' @param ... a list of vectors/lists of length > 0
#' @param paropts a list of parameters to be handed to 
#'    \code{mclapply} (see details)
#'    
#' @details mcZipWith discards excess elements without warning: for example 
#' list (1, 2), list (3, 4, 5) becomes list (f( list(1, 3) ), f( list(2, 4) )). 
#' 
#' @seealso see \code{\link{mclapply}} for more details about the parallel
#'     backend being employed. 
#'    
#' @keywords mcZipWith

lists_from_variadic <- function (args) {
	# extract
	
	if (!is.null(names(args))) {
		
		if ('paropts' %in% names(args)) args$paropts <- NULL
		if ('f' %in% names(args)) args$f <- NULL
		
	}
	
	sapply (
		args,
		function (arg) {
			# ensure all 'lists' aren't factors
			
			if (inherits(arg, 'factor') || 
				!(inherits(arg, 'list') || inherits(arg, 'vector'))) {
					stop ('factor, non-vector or non-list passed as argument:', arg)	
			}
	} )
}

mcZipWith <- function (f, ..., paropts = NULL) {
	# takes n lists/vectors, generates a list of n-tuples. 
	# returns the result of mapping f over this new list. 
	# excess elements are discarded. 
	
	# list (x1, x2), list (y1, y2)  |-> list ( list(x1, y1), list(x2, y2) )
	
	args <- Map (eval, as.list(match.call())[-1]) # force evaluation

	if (length(args) == 0) return (NULL)

	lists <- lists_from_variadic(args)

	f <- match.fun(f)
	
	shortest <- min(sapply(lists, length))
		
	to_zip <- lapply (
		lists, function (x) x[seq_len(shortest)] )
 
	zipped <- call_mclapply (
		f = function (ind) {
			lapply (to_zip, function (x) x[[ind]])
		},	
		x = seq_len(shortest), 
		paropts)
	
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
#' @param ... a list of vectors/lists of length > 0
#' @param paropts a list of parameters to be handed to 
#'    \code{mclapply} (see details)
#'    
#' @details mcZip discards excess elements without warning: for example 
#' list (1, 2), list (3, 4, 5) becomes list (list(1, 3), list(2, 4)). 
#' 
#' @seealso see \code{\link{mclapply}} for more details about the parallel
#'     backend being employed, \code{\link{mcUnzip}} for the inverse of 
#'     this function, and \code{\link{mcZipWith}} for a more general version 
#'     of this function. 
#'    
#' @keywords mcZip

mcZip <- function(..., paropts) {
	# special case of mcZipWith: applies identity to result
	
	mcZipWith (identity, ..., paropts)

}