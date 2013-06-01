#' @title mcAll
#' 
#' @export
#' @description mcAny checks if a predicate function f is true for all 
#' elements in the list or vector x
#' 
#' @details NA's obtained while applying f to x will be assumed to be FALSE.
#' the user can sidestep this behaviour easily, if necessary (see the vignette). 
#' 
#' @name mcAll
#' 
#' @param f a unary function that returns a boolean value, or a string
#' giving the name of a function.
#' @param x a list or vector.
#' @param paropts paropts a list of parameters to be handed to 
#'    mclapply (see the vignette).
#'    
#' @keywords mcAll
#' @example inst/examples/examples-all.r
#' 
#' @return returns TRUE if f is true for one element in x, 
#' otherwise it returns FALSE. If x is NULL, NULL is returned. If x is an
#' empty list an empty list is returned.

mcAll <- function (f, x, paropts = NULL) {
	# apply a function f to x, return TRUE iff f is true
	# for all x
	
	func_call <- paste0( deparse(match.call()), ':' )

	f <- match.fun(f)
	if (is.null(x)) return(x)
	if (is.list(x) && length(x) == 0) return(list())
	is.factor(x) %throws% stopf (
		'%s x may not be a factor; actual value was %s (%s)',
		func_call, deparse(x), paste0(class(x), collapse = ', '))
	
	bools <- as.logical(call_mclapply(f, x, paropts))
	bools[is.na(bools)] <- FALSE
	
	all(bools)
}

#' @title mcAny
#' 
#' @export
#' @description mcAny checks if a predicate function f is true for one or more 
#' elements in the list or vector x
#' 
#' @details NA's obtained while applying f to x will be assumed to be FALSE.
#' the user can sidestep this behaviour easily, if necessary (see the vignette).
#' 
#' @name mcAny
#' 
#' @param f a unary function that returns a boolean value, or a string
#' giving the name of a function.
#' @param x a list or vector.
#' @param paropts paropts a list of parameters to be handed to 
#'    mclapply (see the vignette).
#'    
#' @keywords mcAny
#' @example inst/examples/examples-any.r
#'
#' @return returns TRUE if f is true for one element in x, 
#' otherwise it returns FALSE. If x is NULL, NULL is returned. If x is an
#' empty list an empty list is returned.
#'

mcAny <- function (f, x, paropts = NULL) {
	# apply a function f to x, return TRUE iff f is true
	# for any x
	
	func_call <- paste0( deparse(match.call()), ':' )

	f <- match.fun(f)
	if (is.null(x)) return(x)
	if (is.list(x) && length(x) == 0) return(list())
	is.factor(x) %throws% stopf (
		'%s x may not be a factor; actual value was %s (%s)',
		func_call, deparse(x), paste0(class(x), collapse = ', '))

	
	
	FALSE
}

#' @title mcOne
#' 
#' @export
#' @description mcOne checks if a predicate function f is true for exactly one 
#' element in the list or vector x
#' 
#' @details NA's obtained while applying f to x will be assumed to be FALSE.
#' the user can sidestep this behaviour easily, if necessary (see the vignette).
#' 
#' @name mcOne
#' 
#' @param f a unary function that returns a boolean value, or a string
#' giving the name of a function.
#' @param x a list or vector.
#' @param paropts paropts a list of parameters to be handed to 
#'    mclapply (see the vignette).
#'
#' @return returns TRUE if f is true for one element in x, 
#' otherwise it returns FALSE. If x is NULL, NULL is returned. If x is an
#' empty list an empty list is returned.
#'
#' @example inst/examples/examples-one.r
#' @keywords mcOne
#' 

mcOne <- function (f, x, paropts = NULL) {
	# apply a function f to x, return TRUE iff f is true
	# for one x
	
	func_call <- paste0( deparse(match.call()), ':' )

	f <- match.fun(f)
	
	if (is.null(x)) return(x)
	if (is.list(x) && length(x) == 0) return(list())
	is.factor(x) %throws% stopf (
		'%s x may not be a factor; actual value was %s (%s)',
		func_call, deparse(x), paste0(class(x), collapse = ', '))
	
	bools <- as.logical(call_mclapply(f, x, paropts))
	bools[is.na(bools)] <- FALSE
	
	length(which(bools)) == 1
}
