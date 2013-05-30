#' @description mcUnzipWith is the inverse function of mcZipWith; it takes
#' a list of n element lists, makes returns n lists and applies a function to
#' these lists before returning these lists
#'
#' @title mcUnzipWith
#' 
#' @export
#' @param f a function that takes a single n-element list, or a string
#' giving the name of such a function.
#' @param x a list of lists or vectors
#' @param paropts paropts a list of parameters to be handed to 
#'    mclapply (see the vignette).
#'    
#' @details list names are dropped without warning during unzipping; an example below shows how to add
#' names to the output list. NULL elements in x are automatically removed from x. The empty list is not 
#' removed in order act as a 'zero' to preserve useful structural identities.
#' 
#' the input lists are assumed to be of equal length; if they are not excess elements are discarded
#' without warning.
#' 
#' @seealso see \code{\link{mcZipWith}} for the inverse of 
#' this function and \code{\link{mcUnzip}} for a shorthand variant of this function with
#' f set to identity.
#' 
#' @keywords mcUnzipWith
#' @example inst/examples/examples-unzipwith.r

mcUnzipWith <- function (f, x, paropts = NULL) {
	# rough inverse of mcZipWith: mcUnzipWith ( mcZipWith (x) ) |-> x 
	
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
	
	shortest_tuple <- min(sapply(x, length))
	
	to_unzip <- Map (
		function (elem) elem[seq_len(shortest_tuple)], 
		lists)
	
	unzipped <- call_mclapply (
		f = function (ind) {
			lapply (to_unzip, function (x) x[[ind]])
		},
		x = seq_len(shortest_tuple), 
		paropts)
	
	call_mclapply (
		f = f,	
		x = unzipped,
		paropts )
}

#' @description mcUnzip is the inverse function of mcZip; it takes a list 
#' of n element vectors, and returns n lists. 
#' mcUnzip (mcZip (x)) |-> x
#' 
#' @title mcUnzip
#' @author Ryan Grannell
#' 
#' @export
#' @param x a list of lists or vectors
#' @param paropts paropts a list of parameters to be handed to 
#'    mclapply (see the vignette).
#' 
#' @details list names are dropped without warning during unzipping; an example below shows how to add
#' names to the output list. NULL elements in x are automatically removed from x. The empty list is not 
#' removed in order act as a 'zero' to preserve useful structural identities.
#' 
#' the input lists are assumed to be of equal length; if they are not excess elements are discarded
#' without warning.
#' 
#' @seealso see \code{\link{mcZip}} for the inverse of this function, 
#' \code{\link{mcUnzipWith}} for a more general version of this function.
#'    
#' @keywords mcUnzip
#' @example inst/examples/examples-unzip.r

mcUnzip <- function (x, paropts = NULL) {
	# inverse of mcZip

	mcUnzipWith(f = identity, x, paropts = paropts)
	
}
