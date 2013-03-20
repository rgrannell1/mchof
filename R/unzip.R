#' @description mcUnzip is the inverse function of mcZip. 
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
 
mcUnzip <- function (..., paropts) {
	# rough inverse of mcZip: mcUnzip ( mcZip (x) ) |-> x 
	
	# list ( list(x1, y1), list(x2, y2) ) |-> list ( list (x1, x2), list (y1, y2) )
	
	args <- lapply (as.list(match.call())[-1], eval)
	
	if (length(args) == 0) return (NULL)
	
	if (!is.null(names(args))) {
		
		if ('paropts' %in% names(args)) args$paropts <- NULL
		if ('f' %in% names(args)) args$f <- NULL
		
	}
	
	shortest <- min(sapply(args, length))	
	
	# ensure that factors aren't accepted inappropriately
	
	to_unzip <- lapply (
		args, function (x) x[seq_len(shortest)] )
	types <- lapply (
		args,	
		function (x) 1 )  
	
	call_mclapply (
		f = function (ind) {
			lapply (to_zip, function (x) x[[ind]])
		},	
		x = seq_len(shortest), 
		paropts)
	
}