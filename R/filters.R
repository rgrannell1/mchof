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
	
	func_call <- "mcFilter(f, x, paropts = NULL)"

	missing(f) %throws% messages$function_is_required(func_call, "f")
	missing(x) %throws% messages$vector_is_required(func_call, "g")
	
	f <- match.fun(f)
	if (length(x) == 0) return (x)
	is.factor(x) %throws% messages$was_factor(func_call, x, "x")
	
	ind <- as.logical(unlist(call_mclapply(f, x, paropts, func_call)))
	true_ind <- !is.na(ind) & ind
	
	x[true_ind]	
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
	
	func_call <- "mcReject(f, x, paropts = NULL)"

	missing(f) %throws% messages$function_is_required(func_call, "f")
	missing(x) %throws% messages$vector_is_required(func_call, "x")
		
	f <- match.fun(f)
	g <- function (...) {
		res <- as.logical(f(...))
		!isTRUE(res)
	}

	if (length(x) == 0) return (x)
	is.factor(x) %throws% messages$was_factor(func_call, x, "x")
	
	ind <- as.logical(unlist(call_mclapply(f, x, paropts, func_call)))
	true_ind <- !is.na(ind) & ind
	
	x[!true_ind]	
}

#' @rdname mchof_filters
#' @family mchof-filters
#' @export

mcPartition <- function (f, x, paropts = NULL) {
	# (a -> boolean) -> [a] -> [a]
	# returns two lists; a list for which f returns 
	# true, and a list for which f returns false
	
	func_call <- "mcPartition(f, x, paropts = NULL)"
	
	missing(f) %throws% messages$function_is_required(func_call, "f")
	missing(x) %throws% messages$vector_is_required(func_call, "x")
	
	f <- match.fun(f)
	if (is.null(x)) return (NULL)
	is.factor(x) %throws% messages$was_factor(func_call, x, "x")
	
	ind <- as.logical( unlist(call_mclapply(f, x, paropts, func_call)) )
	true_ind <- !is.na(ind) & ind
	
	list (x[true_ind], x[!true_ind])
}

