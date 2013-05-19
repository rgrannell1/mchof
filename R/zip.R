#' @description mcZipWith takes n lists/vectors, generates a list of n element lists,
#' and returns the result of mapping f over this new list. 
#' 
#' @title mcZipWith
#' @author Ryan Grannell
#' 
#' @export
#' @param f a function that takes a single n-element list
#' @param x a list of lists
#' @param paropts a list of parameters to be handed to 
#'    \code{mclapply} (see details)
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
#' @examples 
#' # adding indices to a list
#' mcZipWith (
#'     function (x) {
#'         list(x[[2]], ind = x[[1]])
#'     }, list (list(1:10), list(letters[1:10]))
#' )
#' 
#' # adding names to output
#' mcZipWith (
#'     function (x) {
#'         list(id = x[[1]] , name = x[[2]])
#'     }, 
#'     list ( list('001', '002', '003'),
#'     list('John', 'Jane', 'Jill')))   
#' @keywords mcZipWith

mcZipWith <- function (f, x, paropts = NULL) {
	# takes n lists/vectors, generates a list of n-tuples. 
	# returns the result of mapping f over this new list. 
	# excess elements are discarded. 
	
	# list (x1, x2), list (y1, y2)  |-> 
	# list ( list(x1, y1), list(x2, y2) )
	
	f <- match.fun(f)
	if (is.null(x)) return (NULL)
	if (is.list(x) && length(x) == 0) return (list())
	
	lists <- Filter(
		function (li) {

			if (inherits(li, 'factor')) stop('factors are not allowed')
			!is.null(li) && any(c('list', 'vector') %in% is(li))

		}, x)

	shortest <- min(sapply (lists, length))

	if (shortest == 0) {
		return(list())
	}
	
	to_zip <- Map (
		function (li) li[seq_len(shortest)], 
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
#'    \code{mclapply} (see details)
#'    
#' @details mcZip discards excess elements without warning: for example 
#' list (1, 2), list (3, 4, 5) becomes list (list(1, 3), list(2, 4)). 
#' 
#' @seealso see \code{\link{mclapply}} for more details about the parallel
#'     backend being employed, \code{\link{mcUnzip}} for the inverse of 
#'     this function, and \code{\link{mcZipWith}} for a more general version 
#'     of this function. 
#' @examples 
#' # zip a name list and an id list into name:id pairs
#' mcZip (list( list('Jack', 'Jane', 'Joe'), list(1, 2, 3)))
#' 
#' @keywords mcZip

mcZip <- function(x, paropts = NULL) {
	# special case of mcZipWith: applies identity to result
	
	mcZipWith (f = identity, x, paropts = paropts)

}
