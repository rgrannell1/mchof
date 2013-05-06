#' @description mcUnzipWith is the inverse function of mcZipWith; it takes
#' a list of n element lists, makes returns n lists and applies a function to
#' these lists before returning these lists
#'
#' @title mcUnzipWith
#' @author Ryan Grannell
#' 
#' @export
#' @param f a function that takes a single n-element list
#' @param x a list of lists
#' @param paropts a list of parameters to be handed to 
#'    \code{mclapply} (see details)
#'    
#' @details Names are 
#' dropped without warning during unzipping; named outputs are given in the 
#' example below
#' 
#' @seealso see \code{\link{mclapply}} for more details about the parallel
#' backend being employed, \code{\link{mcZipWith}} for the inverse of 
#' this function and \code{\link{mcUnzip}} for a variant of this function.
#' 
#' @keywords mcUnzipWith

mcUnzipWith <- function (f, x, paropts = NULL) {
	# rough inverse of mcZipWith: mcUnzipWith ( mcZipWith (x) ) |-> x 
	
	f <- match.fun(f)
	if (is.null(x)) return (NULL)
	if (is.list(x) && length(x) == 0) return (list())
	
	lists <- Filter(
		function (li) {
			
			if (inherits(li, 'factor')) stop('factors are not allowed')
			
			!is.null(li) && any(c('list', 'vector') %in% is(li))
		}, x)
	
	shortest_tuple <- min(sapply(x, length))
	
	to_unzip <- Map (
		function (li) li[seq_len(shortest_tuple)], 
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
#' @param x a list of lists
#' @param paropts a list of parameters to be handed to 
#' \code{mclapply} (see details)
#'    
#' @details mcUnzip discards excess elements, as with mcZip. 
#' 
#' @seealso see \code{\link{mcZip}} for the inverse of this function, 
#' \code{\link{mcUnzipWith}} for a more general version of this function and 
#' \code{\link{mclapply}} for more details about the parallel backend being employed. 
#'    
#' @keywords mcUnzip

mcUnzip <- function (x, paropts = NULL) {
	# inverse of mcZip

	mcUnzipWith(f = identity, x, paropts = paropts)
	
}
