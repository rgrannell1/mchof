#'   
#' @title mcZipWith
#' 
#' @description mcZipWith takes n lists or vectors, generates a list of n element lists,
#' and returns the result of mapping f over each n element list. 
#' 
#' @export
#' 
#' @param f a function that takes n arguments, or a string
#' giving the name of such a function.
#' @param ... several lists or vectors.
#' @param paropts a list of parameters to be handed to 
#'    mclapply (see \link{mchof}).
#'    
#' @return returns the result of mapping f over a list of n element lists.
#'    
#' @details list names are dropped without warning during zipping; an example below shows how to add
#' names to the output list. NULL elements in x are automatically removed from x. The empty list is not 
#' removed in order act as a 'zero' to preserve useful structural identities.
#' 
#' the input lists are assumed to be of equal length; if they are not excess elements are discarded
#' without warning.
#' 
#' @seealso see \code{\link{mcZip}} for a varient of this function
#'     and \code{\link{mcUnzipWith}} for the inverse of this function.
#' @keywords mcZipWith
#' @example inst/examples/examples-zipwith.r

mcZipWith <- function (f, ..., paropts = NULL) {
	# takes n lists/vectors, generates a list of n-tuples. 
	# returns the result of mapping f over this new list. 
	# excess elements are discarded. 
	
	func_call <- "mcZipWith(f, ..., paropts = NULL)"
	
	missing(f) %throws% messages$function_is_required(func_call, "f")
	
	x <- list(...)
	f <- match.fun(f)

	if (is_list0(x)) return (list())
	if (is.null( x[[1]] )) return (NULL)
	
	sublist_info <- sapply(x, function (elem) {
		c(
			factor = inherits(elem, "factor"),
			not_null = !is.null(elem),
			length = length(elem))
	})
	
	any(sublist_info["factor",]) %throws% 
		messages$these_were_factors(func_call,
		paste0(which(sublist_info["factor",]), collapse = ", "), "x")

	min_length <- min(sublist_info["length",])
	
	if (min_length == 0) return (list())

	which_not_null <- which(sublist_info["not_null",] == 1)
	
	if (length(which_not_null) == 0) return (NULL)
	
	x <- x[which_not_null]
	
	call_mclapply(
		function (ind) {
			# get the ind-th element in each sublist,
			# add to a list, apply f to that list
			
			do.call(f, lapply( x, function (sublist) sublist[[ind]] ))
		},
		seq_len(min_length),
		paropts
	)
}

#' @description mcZip takes n lists/vectors, and generates a list of n element lists.
#' It is a special case of mcZipWith
#' 
#' @title mcZip
#' 
#' @export
#' @param ... several lists or vectors.
#' @param paropts a list of parameters to be handed to 
#'    mclapply (see \link{mchof}).
#' 
#' @return returns a list of n element lists.
#'    
#' @details list names are dropped without warning during zipping; an example below shows how to add
#' names to the output list. NULL elements in x are automatically removed from x. The empty list is not 
#' removed in order act as a 'zero' to preserve useful structural identities.
#' 
#' the input lists are assumed to be of equal length; if they are not excess elements are discarded
#' without warning.
#' 
#' @seealso see \code{\link{mcUnzip}} for the inverse of 
#'     this function, and \code{\link{mcZipWith}} for a more general version 
#'     of this function.
#' 
#' @keywords mcZip
#' @example inst/examples/examples-zip.r

mcZip <- function(..., paropts = NULL) {
	# special case of mcZipWith: applies identity to result

	var_identity <- function (...) list(...)
	mcZipWith(f = var_identity, ..., paropts = paropts)

}

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
