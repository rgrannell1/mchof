#' 
#' @description mcFilter extracts the elements of a vector for 
#' which the function \code{f} returns \code{TRUE}, in parallel
#' 
#' @title mcFilter
#' 
#' @export
#' @param f a unary function that returns either \code{TRUE} or \code{FALSE}
#' @param x a vector
#' @param paropts a list of parameters to be handed to 
#'    mclapply (see details and \code{\link{mclapply}})
#'    
#' @details mcFilter behaves identically to \code{\link{Filter}} from the user's 
#'     point of view; the results of the function should not differ from those obtained
#'     using \code{\link{Filter}} with similar parameters. However, execution 
#'     should be much faster for particular application than Filter, 
#'     especially if the function \code{f} is computation-intensive and a large
#'     number of cores are available. 
#'     
#'     As with \code{\link{Filter}}, NA values obtained during filtering are 
#'     assumed to be \code{FALSE}
#' 
#' @seealso see \code{\link{Filter}} for the non-parallel version of this 
#'     function, \code{\link{mclapply}} for more details about the parallel
#'     backend being employed. 
#'    
#' @examples
#' # remove NA values from a vector 
#' p <- function(x) !is.na(x)
#' mcFilter(p, c(3,2,6,NA, 2))
#' 
#' # the same example, in parallel
#' p <- function(x) !is.na(x)
#' mcFilter(p, c(3,2,6,NA, 2, list(mc.cores = 2)))
#' 
#' # a more advanced example, using anonymous functions to
#' # filter out combinations that don't meet a predicate 
#' mcFilter(
#'     f = function(pair){
#'         val <- sum(unlist(pair))
#'  	   if(val > 2 && val < 10) TRUE else FALSE
#'     }, x = apply(combn(8, 3), 2, list)) 

mcFilter <- function(f, x, paropts = NULL){
	# multicore version of the Filter function
	
	if(is.null(x)) return(x)
	
	ind <- as.logical(call_mclapply(f, x, paropts))
	x[!is.na(ind) & ind]
}

# require(devtools); require(testthat)
# auto_test(
# 	'/home/rgrannell1/Dropbox/R directory/mchof/R/filter.R',
# 	'/home/rgrannell1/Dropbox/R directory/mchof/tests/testmcFilter.R')
