#' @title mcAll
#' 
#' @export
#' @description mcAny checks if a predicate function f is true for all 
#' elements in the list or vector x
#' 
#' @details NA's obtained while applying f to x will be assumed to be FALSE.
#' the user can sidestep this behaviour easily, if necessary (see \link{mchof}).
#' mcAll is TRUE for list() and length zero vectors for consistency with the 
#' base function all(). 
#' 
#' @name mcAll
#' 
#' @param f a unary function that returns a boolean value, or a string
#' giving the name of such a function.
#' @param x a list or vector.
#' @param paropts a list of parameters to be handed to 
#'    mclapply (see \link{mchof}).
#'    
#' @keywords mcAll
#' @example inst/examples/examples-all.r
#' 
#' @return returns TRUE if f is true for one element in x, 
#' otherwise it returns FALSE. If x is NULL, NULL is returned. If x is an
#' empty list or vector TRUE is returned.

mcAll <- function (f, x, paropts = NULL) {
	# apply a function f to x, return TRUE iff f is true
	# for all x
	
	func_call <- paste0( deparse(match.call()), ':' )
	
	missing(f) %throws% stopf (
		'%s a function (or function name) f is required but was missing',
		func_call)
	missing(x) %throws% stopf (
		'%s list/vector x is required but was missing',
		func_call)
	
	f <- match.fun(f)
	if (is.null(x)) return (x)
	if (length(x) == 0) return (TRUE)

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
#' the user can sidestep this behaviour easily, if necessary (see \link{mchof}).
#' 
#' @name mcAny
#' 
#' @param f a unary function that returns a boolean value, or a string
#' giving the name of such a function.
#' @param x a list or vector.
#' @param paropts a list of parameters to be handed to 
#'    mclapply (see \link{mchof}).
#'    
#' @keywords mcAny
#' @example inst/examples/examples-any.r
#'
#' @return returns TRUE if f is true for one element in x, 
#' otherwise it returns FALSE. If x is NULL, NULL is returned. If x is an
#' empty list or vector FALSE is returned.
#'

mcAny <- function (f, x, paropts = NULL) {
	# apply a function f to x, return TRUE iff f is true
	# for any x
	
	func_call <- paste0( deparse(match.call()), ':' )
	
	missing(f) %throws% stopf (
		'%s a function (or function name) f is required but was missing',
		func_call)	
	missing(x) %throws% stopf (
		'%s list/vector x is required but was missing',
		func_call)
	
	f <- match.fun(f)
	if (is.null(x)) return (NULL)
	if (length(x) == 0) return (FALSE)
	is.factor(x) %throws% stopf (
		'%s x may not be a factor; actual value was %s (%s)',
		func_call, deparse(x), paste0(class(x), collapse = ', '))
	
	cores <- get_cores(paropts)

	results <- call_mclapply(
		f = function (sublist) {
			
			for (ind in seq_along(sublist)) {
				res <- as.logical(f( sublist[[ind]] ))
				if (isTRUE(res)) return (TRUE)
			}
			FALSE
		},
		group_into(x, cores), paropts
	)
	any(unlist(results))
}

#' @title mcOne
#' 
#' @export
#' @description mcOne checks if a predicate function f is true for exactly one 
#' element in the list or vector x
#' 
#' @details NA's obtained while applying f to x will be assumed to be FALSE.
#' the user can sidestep this behaviour easily, if necessary (see \link{mchof}).
#' 
#' @name mcOne
#' 
#' @param f a unary function that returns a boolean value, or a string
#' giving the name of such a function.
#' @param x a list or vector.
#' @param paropts a list of parameters to be handed to 
#'    mclapply (see \link{mchof}).
#'
#' @return returns TRUE if f is true for one element in x, 
#' otherwise it returns FALSE. If x is NULL, NULL is returned. If x is an
#' empty list or vector FALSE is returned.
#'
#' @example inst/examples/examples-one.r
#' @keywords mcOne
#' 

mcOne <- function (f, x, paropts = NULL) {
	# apply a function f to x, return TRUE iff f is true
	# for one x
	
	func_call <- paste0( deparse(match.call()), ':' )

	missing(f) %throws% stopf (
		'%s a function (or function name) f is required but was missing',
		func_call)
	missing(x) %throws% stopf (
		'%s list/vector x is required but was missing',
		func_call)
	
	f <- match.fun(f)
	if (is.null(x)) return (NULL)
	if (length(x) == 0) return (FALSE)

	is.factor(x) %throws% stopf (
		'%s x may not be a factor; actual value was %s (%s)',
		func_call, deparse(x), paste0(class(x), collapse = ', '))

	cores <- get_cores(paropts)

	number_true <- 0
	job_indices <- group_into(seq_along(x), cores)

	for (i in seq_along(job_indices)) {
		
		if (number_true > 1) return (FALSE)
		
		bools <- unlist(call_mclapply(
			f = function (ind) {
				# returns indices satisfying f
				
				isTRUE( as.logical(f( x[[ind]] )) )
			},
			x = job_indices[[i]],
			paropts
		))
		number_true <- number_true + length(which(bools))
	}
	number_true == 1
}
