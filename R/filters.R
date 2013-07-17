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

mcFilter <- function (f, x, paropts = NULL) {
	# (a -> boolean) -> [a] -> [a]
	# returns x[i] such that f(x[i]) is true
	
	pcall <- sys.call()	

	require_a(c('function', 'string'), f, pcall)
	require_a("listy", x, pcall)
	require_a("listy", paropts, pcall)
	
	f <- match.fun(f)
	require_a('unary function', f, pcall)

	if (length(x) == 0) {
		x
	} else {
		ind <- as.logical(unlist(call_mclapply(f, x, paropts, pcall)))
		true_ind <- !is.na(ind) & ind
		
		x[true_ind]	
	}
}

#' @rdname mchof_filters
#' @family mchof-filters
#' @export

mcSelect <- mcFilter

#' @rdname mchof_filters
#' @family mchof-filters
#' @export

mcReject <- function (f, x, paropts = NULL) {
	# (a -> boolean) -> [a] -> [a]
	# returns x[i] such that f(x[i]) is false
	
	pcall <- sys.call()	

	require_a(c('function', 'string'), f, pcall)
	require_a("listy", x, pcall)
	require_a("listy", paropts, pcall)
	
	f <- match.fun(f)
	
	require_a('unary function', f)	

	if (length(x) == 0) {
		x
	} else {
		ind <- as.logical(unlist(call_mclapply(f, x, paropts, pcall)))
		true_ind <- !is.na(ind) & ind
		
		x[!true_ind]			
	}
}

#' @rdname mchof_filters
#' @family mchof-filters
#' @export

mcPartition <- function (f, x, paropts = NULL) {
	# (a -> boolean) -> [a] -> [a]
	# returns two lists; a list for which f returns 
	# true, and a list for which f returns false
	
	pcall <- sys.call()	
	require_a(c('function', 'string'), f, pcall)
	require_a("listy", x, pcall)
	require_a("listy", paropts, pcall)
	
	f <- match.fun(f)
		require_a("unary function", f, pcall)

	ind <- as.logical( unlist(call_mclapply(f, x, paropts, pcall)) )
	true_ind <- !is.na(ind) & ind
	
	list (x[true_ind], x[!true_ind])
}

