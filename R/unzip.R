#' @description mcUnzip is the inverse function of mcZip; it takes a list 
#' of n element vectors, and returns n lists. 
#' mcUnzip (mcZip (x)) |-> x
#' 
#' @title mcUnzip
#' @author Ryan Grannell
#' 
#' @export
#' @param ... a list of vectors/lists of length > 0
#' @param paropts a list of parameters to be handed to 
#' \code{mclapply} (see details)
#'    
#' @details mcUnzip discards excess elements, as with mcZip. 
#' 
#' @seealso see \code{\link{mcZip}} for the inverse of this function, and 
#' \code{\link{mclapply}} for more details about the parallel backend being employed. 
#'    
#' @keywords mcUnzip
 
mcUnzip <- function (x, paropts) {
	# rough inverse of mcZip: mcUnzip ( mcZip (x) ) |-> x 
	
	lists <- Filter(
		function (li) {
			
			if (inherits(li, 'factor')) stop('factors are not allowed')
			
			!is.null(li) && any(c('list', 'vector') %in% is(li))
			
		}, x)
	
	call_mclapply (
		f = function (ind) {
			lapply (to_zip, function (x) x[[ind]])
		},	
		x = seq_len(shortest), 
		paropts)
	
}
