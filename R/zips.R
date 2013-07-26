
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
#' below shows how to add names to the output list.
#' 
#' the input lists are assumed to be of equal length; if they are not excess elements are discarded
#' without warning.

#' @keywords mcZipWith, mcZip, mcUnzip, mcUnzipWith
#' @example inst/examples/examples-zips.r

#' @rdname mchof_zip
#' @family mchof-zip
#' @export

mcZipWith <- function (f, ..., paropts = NULL) {
	"takes n lists/vectors, generates a list of n-tuples. 
	 returns the result of mapping f over this new list. 
	 excess elements are discarded."
	
	pcall <- sys.call()

	require_a("functionable", f, pcall)
	require_a(c("named list", "named pairlist"), paropts, pcall)
	
	xs <- list(...)
	f <- match.fun(f)
	min_length <- min(vapply(xs, length, 1))

	if (length(xs) == 0 || min_length == 0) {
		list()
	} else {
		call_mclapply(
			function (ind) {

				tuple <- lapply( xs, function (li) li[[ind]] )
				do.call(f, tuple)
			},
			seq_len(min_length),
			paropts, pcall
		)		
	}
}

#' @rdname mchof_zip
#' @family mchof-zip
#' @export

mcZip <- function(..., paropts = NULL) {
	"special case of mcZipWith: applies identity to result"
 
	mcZipWith(function (...) list(...), ..., paropts = paropts)

}

#' @rdname mchof_zip
#' @family mchof-zip
#' @export

mcUnzipWith <- function (f, xs, paropts = NULL) {
	"takes a list of n-tuples, returns n lists
	 returns the result of mapping f over this new list. 
	 excess elements are discarded."

	pcall <- sys.call()
	
	require_a("functionable", f, pcall)
	require_a("listy", xs, pcall)
	require_a(c("named list", "named pairlist"), paropts, pcall)

	f <- match.fun(f)
	min_length <- min(vapply(xs, length, 1))
	
	if (length(xs) == 0 || min_length == 0) {
		list()
	} else {
		call_mclapply(
			function (ith) {

				tuple <- lapply( xs, function (li) li[[ith]] )
				do.call(f, tuple)
			},
			seq_len(min_length),
			paropts, pcall
		)
	}
}

#' @rdname mchof_zip
#' @family mchof-zip
#' @export

mcUnzip <- function (xs, paropts = NULL) {
	"inverse of mcZip"

	mcUnzipWith(function (...) list(...), xs, paropts = paropts)	
}
