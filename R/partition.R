
#' @description mcPartition takes n lists/vectors, generates a list of n element lists,
#' and returns the result of mapping f over this new list. 
#' 
#' @title mcPartition
#' @author Ryan Grannell
#' 
#' @export
#' @param f a unary function that returns a boolean value
#' @param x a vector or list
#' @param paropts a list of parameters to be handed to 
#'    \code{mclapply} (see details)
#'    
#' @details NA values obtained during logical filtering are assumed to be FALSE,
#' as with other functions in this package. The user can modify this behaviour
#' by making sure the argument f returns TRUE is a value is NA under coersion. 
#' 
#' @seealso see \code{\link{mclapply}} for more details about the parallel
#' backend being employed, and \code{\link{mcFilter}} for a function that returns
#' only the values for which f returns TRUE.
#'    
#' @keywords mcPartition
#' 

mcPartition <- function (f, x, paropts = NULL) {
	# returns two lists; a list for which f returns 
	# true, and a list for which f returns false
		
	f <- match.fun(f)
	if (is.null(x)) return(x)
	if (is.factor(x)) stop('x may not be a factor')
	
	ind <- as.logical(call_mclapply(f, x, paropts))

	list (
		true  = x[!is.na(ind) & ind],
		false = x[is.na(ind) | !ind] )

}
