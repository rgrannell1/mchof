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
	# inverse of mcZip: mcUnzip ( mcZip (x) ) |-> x 
	
	
}
