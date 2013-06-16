#' @description mcUnzipWith is the inverse function of mcZipWith; it takes
#' a list of n element lists, makes returns n lists and applies a function to
#' these lists before returning these lists
#'
#' @title mcUnzipWith
#' 
#' @export
#' @param f a function that takes n arguments, or a string giving the name of 
#' such a function.
#' @param x a list of lists or vectors
#' @param paropts a list of parameters to be handed to 
#'    mclapply (see \link{mchof}).
#'    
#' @details list names are dropped without warning during unzipping; an example below shows how to add
#' names to the output list. NULL elements in x are automatically removed from x. The empty list is not 
#' removed in order act as a 'zero' to preserve useful structural identities.
#' 
#' the input lists are assumed to be of equal length; if they are not excess elements are discarded
#' without warning.
#' 
#' @seealso see \code{\link{mcZipWith}} for the inverse of 
#' this function and \code{\link{mcUnzip}} for a shorthand variant of this function with
#' f set to identity.
#' 
#' @keywords mcUnzipWith
#' @example inst/examples/examples-unzipwith.r

mcUnzipWith <- function (f, x, paropts = NULL) {
	# rough inverse of mcZipWith: mcUnzipWith ( mcZipWith (x) ) |-> x 
	# takes a list of n-tuples, returns n lists
	# returns the result of mapping f over this new list. 
	# excess elements are discarded. 

	func_call <- "mcUnzipWith(f, x, paropts = NULL)"
	
	missing(f) %throws% messages$function_is_required(func_call, "f")
	missing(x) %throws% messages$vector_is_required(func_call, "x")
	
	f <- match.fun(f)
	if (length(x) == 0) return (list())

	sublist_info <- sapply(x, function (elem) {
		c(
			factor = inherits(elem, "factor"),
			not_null = !is.null(elem), length = length(elem))
	})

	any(sublist_info["factor",]) %throws% messages$these_were_factors(
		func_call,
		paste0(which(sublist_info["factor",]), collapse = ", "),
		"x")

	min_length <- min(sublist_info["length",])
	
	if (min_length == 0) return (list())

	which_not_null <- which(sublist_info["not_null",] == 1)

	call_mclapply(
		function (ind) {
			# get the ind-th element in each sublist,
			# add to a list, apply f to that list
			
			do.call(f, lapply( x, function (sublist) sublist[[ind]] ))
		},
		seq_len(min_length),
		paropts, func_call
	)
}

#' @description mcUnzip is the inverse function of mcZip; it takes a list 
#' of n element vectors, and returns n lists. 
#' mcUnzip (mcZip (x)) |-> x
#' 
#' @title mcUnzip
#' @author Ryan Grannell
#' 
#' @export
#' @param x a list of lists or vectors
#' @param paropts a list of parameters to be handed to 
#'    mclapply (see \link{mchof}).
#' 
#' @details list names are dropped without warning during unzipping; an example below shows how to add
#' names to the output list. NULL elements in x are automatically removed from x. The empty list is not 
#' removed in order act as a 'zero' to preserve useful structural identities.
#' 
#' the input lists are assumed to be of equal length; if they are not excess elements are discarded
#' without warning.
#' 
#' @seealso see \code{\link{mcZip}} for the inverse of this function, 
#' \code{\link{mcUnzipWith}} for a more general version of this function.
#'    
#' @keywords mcUnzip
#' @example inst/examples/examples-unzip.r

mcUnzip <- function (x, paropts = NULL) {
	# inverse of mcZip
	
	var_identity <- function (...) list(...)
	mcUnzipWith(f = var_identity, x, paropts = paropts)
	
}
