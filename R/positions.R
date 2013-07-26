	
#' Higher-Order-Functions for Finding Values
#'
#' @description 
#' \code{mcPosition} returns the index of the first (or last) position in \code{x} whose value 
#' matches a predicate function \code{f}.
#'  
#' \code{mcFind} returns the value of the first (or last) element of \code{x} that meets the predicate f.  
#'
#' @param f a unary function that returns a boolean value, or a string
#' giving the name of such a function.
#' @param x a list or vector.
#' @param paropts a list of parameters to be handed to 
#'    mclapply (see \link{mchof}).
#'   
#' @example inst/examples/examples-positions.r
#' @keywords mcPosition mcPositionl mcPositionr mcFind mcFindl mcFindr

#' @rdname mchof_positions
#' @family mchof-positions
#' @export

mcPosition <- function (p, xs, paropts = NULL) {
	"returns the first index in x that matches
	the predicate f"

	pcall <- sys.call()
	
	require_a("functionable", p, pcall)
	require_a("listy", xs, pcall)
	require_a(c("named list", "named pairlist"), paropts, pcall)	

	p <- match.fun(p)
	require_a('unary function', p, pcall)
	
	if (length(xs) == 0) {
		integer(0)
	} else {
		cores <- get_cores(paropts)

		if (cores == 1) {
			# if only one core execute sequentially.
			ith <- 1
			while (ith <= length(xs)) {

				if ( as.logical(p( xs[[ith]] )) ) {
					return (ith)
				}
				ith <- ith + 1
			}
			integer(0)
		} else {
			# run in parallel
			job_indices <- group_into(seq_along(xs), cores) 
			
			ith <- 1
			while (ith <= length(job_indices)) {
				
				checked_ind <- unlist(call_mclapply(
					f = function (jth) {
						# check a group of indices at once

						is_match <- p( xs[[jth]] )	
						if (isTRUE(is_match)) {
							jth
						} else {
							NaN
						}
					},
					x = job_indices[[ith]], paropts, pcall
				))
				
				matched_indices <- checked_ind[
					!is.nan(checked_ind) & checked_ind
				]
				
				if (length(matched_indices) > 0) {
					return( min(matched_indices) )
				}
				ith <- ith + 1
			}
			integer(0)
		}
	}
}

#' @rdname mchof_positions
#' @family mchof-positions
#' @export

mcPositionl <- mcPosition

#' @rdname mchof_positions
#' @family mchof-positions
#' @export

mcPositionr <- function (p, xs, paropts = NULL) {
	# returns the last index in x that matches
	# the predicate f

	mcPositionl(p, rev(xs), paropts)
	
}

#' @rdname mchof_positions
#' @family mchof-positions
#' @export

mcFind <- function (p, xs, paropts = NULL) {
	"returns the first (or last) element in x that matches
	the predicate f"

	pcall <- sys.call()

	require_a(c('function', 'string'), p, pcall)
	require_a("listy", xs, pcall)
	require_a(c("named list", "named pairlist"), paropts, pcall)	

	p <- match.fun(p)
	require_a('unary function', p, pcall)
	
	if (length(xs) == 0) {
		xs
	} else {	
		first_match <- mcPosition (p, xs, paropts)
		
		if (!is_integer0(first_match)) {
			s[[first_match]]
		} else {
			integer(0)
		}
	}
}

#' @rdname mchof_positions
#' @family mchof-positions
#' @export

mcFindl <- mcFind

#' @rdname mchof_positions
#' @family mchof-positions
#' @export

mcFindr <- function (p, xs, paropts = NULL) {
	# returns the last index in x that matches
	# the predicate f

	mcFindl(p, rev(xs), paropts)
	
}
