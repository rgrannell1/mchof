
#' Quantifier Functionals
#'
#' @description
#'
#' mcAll checks if a predicate function \code{f} is true for all 
#' elements in the list or vector \code{x}.
#'
#' mcAny checks if a predicate function \code{f} is true for one or more 
#' elements in the list or vector \code{x}.
#'
#' mcOne checks if a predicate function \code{f} is true for exactly one 
#' element in the list or vector \code{x}
#'
#' @param f a unary function that returns a boolean value, or a string
#' giving the name of such a function.
#' @param x a list or vector.
#' @param paropts a list of parameters to be handed to 
#'    mclapply (see \link{mchof}).
#'
#' @details
#'
#' These functions apply \code{f} to each element of \code{x}, and coerce the results to a 
#' \code{TRUE} or \code{FALSE} value. If an \code{NA} value is obtained it is coerced to 
#' \code{FALSE}. This is usually the desired behaviour, but if the user wants
#' \code{NA} values to be converted to \code{TRUE} then they can wrap the input function \code{f} with 
#' \code{\link{mcBoolean}}. For example,
#'
#' \code{all_are_numbers = mcBoolean \%of\% function (x) mcAll(is.numeric, x)}
#'
#' \code{all_are_numbers} first checks whether every \code{x} is numeric,
#' and then coerces this result to \code{TRUE} or \code{FALSE}.
#'
#' All of these functions return \code{NULL} when \code{x = NULL}. 
#' When \code{x} is another length-zero value \code{mcAll} returns \code{TRUE},
#' and \code{mcOne} and \code{mcAny} return \code{FALSE}. The reason that mcAll returns
#' \code{TRUE} for length-zero data is for consistency with the base function \code{all}.
#'
#' @keywords mcAll, mcAny, mcOne
#'
#' @example inst/examples/examples-quantify.r
#' @export
#'
#' @rdname mchof_quantify
#' @family mchof-quantify
#' @export

mcAll <- function (f, x, paropts = NULL) {
	# (a -> bool) -> [a] -> bool
	# apply a function f to x, return TRUE iff f is true
	# for all x

	pcall <- sys.call()
	
	require_a(c('function', 'string'), f, pcall)
	require_a("listy", x, pcall)
	
	f <- match.fun(f)
	require_a('unary function', f, pcall)
	
	if (length(x) == 0) {
		TRUE
	} else {
		bools <- as.logical(call_mclapply(f, x, paropts, pcall))
		bools[is.na(bools)] <- FALSE
		
		all(bools)		
	}
}

#' @rdname mchof_quantify
#' @family mchof-quantify
#' @export

mcAny <- function (f, x, paropts = NULL) {
	# (a -> bool) -> [a] -> bool
	# apply a function f to x, return TRUE iff f is true
	# for any x
	
	pcall <- sys.call(sys.parent())
	
	require_a(c('function', 'string'), f, pcall)
	require_a("listy", x, pcall)
	require_a("listy", paropts, pcall)

	f <- match.fun(f)
	require_a('unary function', f, pcall)
	
	if (length(x) == 0) {
		FALSE
	} else {
		cores <- get_cores(paropts)

		if (cores == 1) {
			return (any( sapply(x, function (el) as.logical(f(el)) ) ))
		}
		
		results <- call_mclapply(
			f = function (sublist) {
				
				for (ind in seq_along(sublist)) {
					res <- as.logical(f( sublist[[ind]] ))
					if (isTRUE(res)) return (TRUE)
				}
				FALSE
			},
			group_into(x, cores), paropts, pcall
		)
		any(unlist(results))		
	}
}

#' @rdname mchof_quantify
#' @family mchof-quantify
#' @export

mcOne <- function (f, x, paropts = NULL) {
	# (a -> bool) -> [a] -> bool
	# apply a function f to x, return TRUE iff f is true
	# for one x
	
	pcall <- sys.call()

	require_a(c('function', 'string'), f, pcall)
	require_a("listy", x, pcall)
	require_a("listy", paropts, pcall)
	
	f <- match.fun(f)
	require_a('unary function', f, pcall)
	
	if (length(x) == 0) {
		FALSE
	} else {
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
				paropts, pcall
			))

			if (length(which(bools)) > 0) {
				number_true <- number_true + length(which(bools))
			}
		}
		number_true == 1
	}
}
