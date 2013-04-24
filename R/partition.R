
#' @description mcPartition returns a list of two lists; a list for which a predicate 
#' returns true, and a list for which a predicate returns false
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
#' @examples
#' # partition a set into even and odd numbers
#' mcPartition ( function (x) x %% 2, 1:10, paropts = list(mc.cores = 2)) 
#' # divide a set of combinations into two based on a predicate
#' mcPartition(
#'	f = function(pair){
#'		val <- sum(unlist(pair))
#'		if (val > 8) TRUE else FALSE
#'	},
#'	x = apply(combn(8, 3), 2, list),
#'	paropts = list(mc.cores = 2))
#' @keywords mcPartition 

mcPartition <- function (f, x, paropts = NULL) {
	# returns two lists; a list for which f returns 
	# true, and a list for which f returns false
		
	f <- match.fun(f)
	if (is.null(x)) return(x)
	if (is.factor(x)) stop('x may not be a factor')
	
	ind <- as.logical(call_mclapply(f, x, paropts))

	list (
		x[!is.na(ind) & ind],
		x[is.na(ind) | !ind])
}
