	
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

mcPosition <- function (f, x, paropts = NULL) {
	# returns the first index in x that matches
	# the predicate f

	pcall <- sys.call(sys.parent())
	
	require_a(c('function', 'string'), f, pcall)
	require_a(c('vector', 'pairlist', 'null'), x, pcall)
	
	f <- match.fun(f)
	
	require_a('unary function', f, pcall)
	
	if (is.null(x)) return (NULL)
	if (length(x) == 0) return (integer(0))

	cores <- get_cores(paropts)

	if (cores == 1) {
		
		ind <- 1
		while (ind <= length(x)) {

			if ( as.logical(f( x[[ind]] )) ) return (ind)
			ind <- ind + 1
		}
		return (integer(0))
	}
 	
	job_indices <- group_into(seq_along(x), cores) 
	
	for (i in seq_along(job_indices)) {
		
		checked_ind <- unlist(call_mclapply(
			f = function (ind) {
				# returns indices satisfying f
				
				is_match <- as.logical(f( x[[ind]] ))	
				if (isTRUE(is_match)) ind else NaN
			},
			x = job_indices[[i]], paropts, pcall
		))
		
		matched_indices <- checked_ind[
			!is.nan(checked_ind) & checked_ind
		]
		
		if (length(matched_indices) > 0) {
			return( min(matched_indices) )
		}
	}
	integer(0)
}

#' @rdname mchof_positions
#' @family mchof-positions
#' @export

mcPositionl <- mcPosition

#' @rdname mchof_positions
#' @family mchof-positions
#' @export

mcPositionr <- function (f, x, paropts = NULL) {
	# returns the last index in x that matches
	# the predicate f

	mcPositionl(f, rev(x), paropts)
	
}

#' @rdname mchof_positions
#' @family mchof-positions
#' @export

mcFind <- function (f, x, paropts = NULL) {
	# returns the first (or last) element in x that matches
	# the predicate f

	pcall <- sys.call(sys.parent())

	require_a(c('function', 'string'), f, pcall)
	require_a(c('vector', 'pairlist', 'null'), x, pcall)
	
	f <- match.fun(f)
	
	require_a('unary function', f, pcall)
	
	if (length(x) == 0) return (x)
	(!is.vector(x)) %throws% 
		messages$class_mismatch(pcall, x, "x", "vector or list")
	
	first_match <- mcPosition (f, x, paropts)
	
	if (!is_integer0(first_match)) {
		x[[first_match]]
	} else integer(0)
}

#' @rdname mchof_positions
#' @family mchof-positions
#' @export

mcFindl <- mcFind

#' @rdname mchof_positions
#' @family mchof-positions
#' @export

mcFindr <- function (f, x, paropts = NULL) {
	# returns the last index in x that matches
	# the predicate f

	mcFindl(f, rev(x), paropts)
	
}
