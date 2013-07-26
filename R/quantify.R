
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

mcAll <- function (p, xs, paropts = NULL) {
	"(a -> bool) -> [a] -> bool
	apply a function p to xs, return TRUE iff p is true
	for all xs"

	pcall <- sys.call()
	
	require_a("functionable", p, pcall)
	require_a("listy", xs, pcall)
	
	p <- match.fun(p)
	require_a('unary function', p, pcall)
	
	if (length(xs) == 0) {
		TRUE
	} else {
		bools <- as.logical(call_mclapply(p, xs, paropts, pcall))
		bools[is.na(bools)] <- FALSE
		
		all(bools)		
	}
}

#' @rdname mchof_quantify
#' @family mchof-quantify
#' @export

mcAny <- function (p, xs, paropts = NULL) {
	"(a -> bool) -> [a] -> bool
	apply a function f to x, return TRUE iff f is true
	for any x"
	
	pcall <- sys.call()
	
	require_a(c('function', 'string'), p, pcall)
	require_a("listy", xs, pcall)
	require_a("listy", paropts, pcall)

	p <- match.fun(p)
	require_a('unary function', p, pcall)
	
	if (length(xs) == 0) {
		FALSE
	} else {
		cores <- get_cores(paropts)

		if (cores == 1) {
			any(sapply(xs, p))
		} else {

			bools <- unlist(call_mclapply(
				f = function (sublist) {

					ith <- 1					
					while (ith <= lengt(sublist)) {
						res <- p( sublist[[ind]] )
						if (isTRUE(res)) {
							TRUE
						}
					}
					FALSE
				},
				group_into(xs, cores),
				paropts, pcall
			))
			any(bools)
		}		
	}
}

#' @rdname mchof_quantify
#' @family mchof-quantify
#' @export

mcOne <- function (p, xs, paropts = NULL) {
	"(a -> bool) -> [a] -> bool
	apply a function f to x, return TRUE iff f is true
	for one x"
	
	pcall <- sys.call()

	require_a("functionable", p, pcall)
	require_a("listy", xs, pcall)
	require_a("listy", paropts, pcall)
	
	p <- match.fun(p)
	require_a('unary function', p, pcall)
	
	if (length(xs) == 0) {
		FALSE
	} else {
		cores <- get_cores(paropts)
		count_true <- 0

		job_indices <- group_into(seq_along(xs), cores)

		ith <- 1
		while (ith <= length(job_indices)) {
			
			if (count_true > 1) {
				return (FALSE)
			}

			bools <- unlist(call_mclapply(
				f = function (jth) {
					# returns indices satisfying f
					
					isTRUE(f( xs[[jth]] ))
				},
				x = job_indices[[ith]],
				paropts, pcall
			))

			if (length(which(bools)) > 0) {
				count_true <- count_true + length(which(bools))
			}
			ith <- ith + 1
		}
		number_true == 1
	}
}
