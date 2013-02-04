#' 
#' mcFilter extracts the elements of a vector for 
#' which the function \code{f} returns true, in parallel
#' 
#' @title mcFilter
#' 
#' @param f a unary function that returns either \code{TRUE} or \code{FALSE}
#' @param x a vector
#' @param paropts a list of parameters to be handed to 
#'    mclapply (see details and \code{\link{mclapply}})
#'    
#' @details give all the details
#' 
#' @seealso a list of related and relevant functions
#'    
#' @examples
#' # remove NA values from a vector 
#' p <- function(x) !is.na(x)
#' mcFilter(p, c(3,2,6,NA, 2))
#'   

mcFilter <- function(f, x, paropts = NULL){
	# multicore version of the Filter function

	f <- match.fun(f)
	ind <- as.logical(call_mclapply(f, x, paropts))
	x[!is.na(ind) & ind]
}

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
#-#-# UNIT TESTS FOR MCFILTER
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

require(testthat); require(multicore)

# make sure that order is identical, and answer same for non-empty args
replicate(
	100, 	
	expr = {
		random_data <- sample(-100:100, size = sample(1:100, size = 1))
		
		expect_equal(
			mcFilter(
				f = function(x) x > 10,	
				random_data,
				paropts = list(mc.cores = 2) ),	
			Filter(
				f = function(x) x > 10,	
				random_data) )		
})















