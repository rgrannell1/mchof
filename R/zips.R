
#' Higher-Order-Functions for Restructuring Lists/Vectors
#'
#' @description
#' 
#' mcZipWith takes n lists or vectors, generates a list of n element lists,
#' and returns the result of mapping \code{f} over each n element list. 
#'
#' mcZip takes n lists/vectors, and generates a list of n element lists.
#' It is a special case of mcZipWith
#'
#' mcUnzipWith is the inverse function of mcZipWith; it takes
#' a list of n element lists, generates n lists and returns
#' the result of mapping zcode{f} over each generated list. 
#'
#' mcUnzip is the inverse function of mcZip; it takes a list 
#' of n element lists/vectors, and returns n lists. It is a special case of mcUnzipWith
#'
#' @param f a function that takes n arguments, or a string giving the name of such a function.
#' @param ... several lists or vectors.
#' @param paropts a list of parameters to be handed to mclapply (see \link{mchof}).
#'    
#' @details list names are dropped without warning during zipping and unzipping; an example 
#' below shows how to add names to the output list. NULL elements in x are automatically 
#' removed from x. The empty list is not removed in order act as a 'zero' to preserve 
#' useful structural identities.
#' 
#' the input lists are assumed to be of equal length; if they are not excess elements are discarded
#' without warning.

#' @keywords mcZipWith, mcZip, mcUnzip, mcUnzipWith
#' @example inst/examples/examples-zips.r

#' @rdname mchof_zip
#' @family mchof-zip
#' @export

mcZipWith <- function (f, ..., paropts = NULL) {
	# takes n lists/vectors, generates a list of n-tuples. 
	# returns the result of mapping f over this new list. 
	# excess elements are discarded. 
	
	func_call <- "mcZipWith(f, ..., paropts = NULL)"
	
	require_a(c('function', 'string'), f)

	x <- list(...)
	f <- match.fun(f)

	x <- Filter(Negate(is.null), x)
	if (length(x) == 0) return (NULL)

	min_length <- min(vapply(x, length, 1))

	call_mclapply(
		function (ind) {

			tuple <- lapply( x, function (li) li[[ind]] )
			do.call(f, tuple)
		},
		seq_len(min_length),
		paropts, func_call
	)
}

#' @rdname mchof_zip
#' @family mchof-zip
#' @export

mcZip <- function(..., paropts = NULL) {
	# special case of mcZipWith: applies identity to result

	var_identity <- function (...) list(...)
	mcZipWith(f = var_identity, ..., paropts = paropts)

}

#' @rdname mchof_zip
#' @family mchof-zip
#' @export

mcUnzipWith <- function (f, x, paropts = NULL) {
	# takes a list of n-tuples, returns n lists
	# returns the result of mapping f over this new list. 
	# excess elements are discarded. 

	func_call <- "mcUnzipWith(f, x, paropts = NULL)"
	
	require_a(c('function', 'string'), f)
	require_a(c('list', 'pairlist', 'null'), x)
	
	f <- match.fun(f)
	if (length(x) == 0) return (list())

	x <- Filter(Negate(is.null), x)
	if (length(x) == 0) return (NULL)

	min_length <- min(vapply(x, length, 1))

	call_mclapply(
		function (ind) {

			tuple <- lapply( x, function (li) li[[ind]] )
			do.call(f, tuple)
		},
		seq_len(min_length),
		paropts, func_call
	)
}

#' @rdname mchof_zip
#' @family mchof-zip
#' @export

mcUnzip <- function (x, paropts = NULL) {
	# inverse of mcZip
	
	var_identity <- function (...) list(...)
	mcUnzipWith(f = var_identity, x, paropts = paropts)
	
}

