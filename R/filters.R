#'
#' @title mcFilter
#' @aliases mcSelect
#' 
#' @description mcFilter extracts the elements of a vector or list for 
#' which the function \code{f} returns \code{TRUE}.
#' 
#' @usage mcFilter(f, x, paropts = NULL)
#' 
#' mcSelect(f, x, paropts = NULL)
#'    
#' This function applies \code{f} to each element of \code{x}, and coerces the results to a 
#' \code{TRUE} or \code{FALSE} value. If an \code{NA} value is obtained this is then coerced to a 
#' \code{FALSE} value. This is usually the desired behaviour, but if the user wants
#' \code{NA} values to be converted to \code{TRUE} then they can wrap the input function \code{f} with 
#' \link{mcBoolean}.
#' 
#' @return returns the elements of x for which f returned true. If x is a list and no elements
#' returned true, returns list(). If x is a vector and no elements returns true, returns a typed 
#' vector of length(0). x = NULL always returns NULL.
#'
#' @template roxygen-filters
#'
#' @section Special Cases:
#'
#' when x is a length-zero value such as NULL or list(), that value is automatically returned.
#'     
#' @example inst/examples/examples-filter.r 
#' @keywords mcFilter mcSelect
#' @export

mcFilter <- function (f, x, paropts = NULL) {
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

#' @export

mcSelect <- mcFilter

#' @title mcReject

#' @description mcReject extracts the elements of a vector or list for  which the function 
#' \code{f} returns \code{FALSE}.
#' 
#' @return returns a list of elements for which f returned FALSE or NA.
#'
#' @details mcReject applies f to each element of x, coerces the result to a logical value,
#' and returns the values for which f returns FALSE.
#' 
#' mcReject is more useful for filtering out NULL or NA values in a list that mcFilter,
#' as is demonstrated in the examples below.
#' 
#' elements for which f returned NA are included, so that concatenating the results of mcFilter and 
#' mcReject will give you the original set x (though unordered). The user can
#' modify this behaviour by making sure the argument f returns TRUE is a value 
#' is NA under coersion, as described in \link{mchof}.
#'
#' @template roxygen-filters
#'
#' @section Special Cases:
#'
#' when x is a length-zero value such as NULL or list(), that value is automatically returned.
#'
#' @example inst/examples/examples-reject.r
#' @export

mcReject <- function (f, x, paropts = NULL) {
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

#' @description mcPartition returns a list of two lists; a list for which a predicate 
#' returns true, and a list for which a predicate returns false.
#' 
#' @title mcPartition
#' 
#' @template roxygen-filters
#'
#' @return returns a list of two lists; the first list contains the values 
#' for which f returned true, the other contains values that returned false or NA. 
#' If the list of true/false elements is empty then the value of that slot is list()
#' if x is a list, and a typed vector such as integer(0) if x is a vector. mcPartition
#' NULL always returns NULL.
#'
#' @section Special Cases:
#'
#' when x is NULL, NULL is returned. If x is a length(0) vector or list,
#' then a list of length two containing that empty value is returned. For example:
#' 
#' list() |-> list( list(), list() )
#'
#' @keywords mcPartition
#' @example inst/examples/examples-partition.r
#' @export

mcPartition <- function (f, x, paropts = NULL) {
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

