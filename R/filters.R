#'
#' Filter-like Higher-Order-Functions
#' 
#' @description 
#' \code{mcFilter} extracts the elements of a vector or list \code{x} for which the function 
#' \code{f} returns \code{TRUE}.
#' 
#' \code{mcSelect} is an alias for \code{mcFilter}.
#'
#' \code{mcReject} extracts the elements of a vector or list \code{x} for which the function 
#' \code{f} returns \code{FALSE}.
#' 
#' \code{mcPartition} returns a list of two lists/vectors; a list for which \code{f}
#' returns \code{TRUE}, and a list for which \code{f} returns \code{FALSE}.
#' 
#' @details 

#' All of these function return \code{NULL} automatically when \code{x} is NULL. \code{mcFilter} and 
#' \code{mcReject} return \code{x} when it is length-zero values automatically.
#'
#' \code{mcReject} also returns elements for which \code{f} returned \code{NA} are included,
#' so that concatenating the results of \code{mcFilter} and 
#' \code{Reject} will give you the original set \code{x} (though probably in the wrong order). 
#'
#' \code{mcPartition} always returns a list of two lists/vectors; the first list/vector contains the values 
#' for which \code{f} returned \code{TRUE}, the other contains values that 
#' returned \code{FALSE} or \code{NA}. When trying to partition a length-zero value a list containing two of that
#' value is returned. For example, when \code{x} is \code{integer(0)} then
#' 
#' \code{list( integer(0), integer(0) )}
#' 
#' is returned. 

#' @example inst/examples/examples-filters.r 
#' @keywords mcFilter mcSelect mcReject mcPartition

#' @rdname mchof_filters
#' @family mchof-filters
#' @export

mcFilter <- function (p, xs, paropts = NULL) {
	"(a -> boolean) -> [a] -> [a]
	 returns x[i] such that f(x[i]) is true."
	
	pcall <- sys.call()	

	require_a("functionable", p, pcall)
	require_a("listy", xs, pcall)
	require_a(c("named list", "named pairlist"), paropts, pcall)
	
	p <- match.fun(p)
	require_a('unary function', p, pcall)

	if (length(xs) == 0) {
		list()
	} else {
		indices <- unlist(call_mclapply(p, xs, paropts, pcall))
		true_indices <- !is.na(indices) & indices
		
		as.list( xs[true_indices] )
	}
}

#' @rdname mchof_filters
#' @family mchof-filters
#' @export

mcSelect <- mcFilter

#' @rdname mchof_filters
#' @family mchof-filters
#' @export

mcReject <- function (p, xs, paropts = NULL) {
	"(a -> boolean) -> [a] -> [a]
	 returns x[i] such that f(x[i]) is false."
	
	pcall <- sys.call()	

	require_a("functionable", p, pcall)
	require_a("listy", xs, pcall)
	require_a(c("named list", "named pairlist"), paropts, pcall)
	
	p <- match.fun(p)
	require_a('unary function', p)	

	if (length(xs) == 0) {
		list()
	} else {
		indices <- unlist(call_mclapply(p, xs, paropts, pcall))
		true_indices <- !is.na(indices) & indices
		
		as.list( xs[!true_indices] )			
	}
}

#' @rdname mchof_filters
#' @family mchof-filters
#' @export

mcPartition <- function (p, xs, paropts = NULL) {
	"(a -> boolean) -> [a] -> [a]
	 returns two lists; a list for which f returns 
	 true, and a list for which f returns false."
	
	pcall <- sys.call()	
	require_a("functionable", p, pcall)
	require_a("listy", xs, pcall)
	require_a(c("named list", "named pairlist"), paropts, pcall)
	
	p <- match.fun(p)
	require_a("unary function", p, pcall)

	indices <- unlist(call_mclapply(p, xs, paropts, pcall))
	true_indices <- !is.na(indices) & indices
	
	list (
		as.list(xs[true_indices]),
		as.list(xs[!true_indices]))
}

