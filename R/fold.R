#' @title mcFold
#' 
#' @export
#' @description mcFold applies an associative binary function to a list,
#' returning a single. The difference between mcFold & mcReduce is that an
#' initial value can be supplied to mcFold, making certain tasks easier (see examples).
#' 
#' @details \code{mcFold} can be used as a parallel replacement for a 
#' subclass of problems that can be solved with the Fold function; parallelising 
#' Fold is only possible when the function f is associative; in infix 
#' notation that is \code{(x1 f x2) f x3} is equivalent to \code{x1 f (x2 f x3)}. 
#' In practicality this means that the order in which the reduce is carried out isn't 
#' important, so several tasks can be carried out in parallel.
#' 
#' A binary function with associativity is the plus operator;
#' \code{1 + (2 + 3) == (1 + 2) + 3}.
#' Subtraction does not have this property \code{1 - (2 - 3) != (1 - 2) - 3},
#' so it should not be used as a binary function for \code{mcFold} 
#' 
#' it is often useful to set first to be the identity of the data type x; for addition
#' this is 0, for character concatentation '' and for list concatenation list() 
#' 
#' @name mcFold
#' 
#' @param f a binary function
#' @param x a vector or list
#' @param paropts paropts a list of parameters to be handed to 
#'    mclapply (see details and \code{\link{mclapply}})
#' @examples
#' 
#'mcFold(
#'	function (acc, new) {
#'		if (new$age > 18) {
#'			c( acc, list(name = new$name) )
#'		} else acc
#'	},
#'	NULL,
#'	list(
#'		list(name = 'John', age = 12),
#'		list(name = 'Daniel', age = 55),
#'		list(name = 'Sasha', age = 24)
#'	),
#'	paropts = list(mc.cores = 2)
#')

mcFold <- function (f, first, x, paropts = NULL) {
	# multicore associative-only version of Fold, with an
	# initial value
	
	mcReduce(f, c(first,x), paropts)
	
}
