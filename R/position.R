
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
#' @param right a boolean value. Should \code{x} be searched starting from the right? Defaults to \code{FALSE}.
#' @param paropts a list of parameters to be handed to 
#'    mclapply (see \link{mchof}).
#'   
#' @example inst/examples/examples-positions.r
#' @keywords mcPosition mcFind

#' @rdname mchof_positions
#' @family mchof-positions
#' @export

mcPosition <- function (f, x, right = FALSE, paropts = NULL) {
	# returns the first (or last) index in x that matches
	# the predicate f
		
	func_call <- "mcPosition(f, x, right=FALSE, paropts=NULL)"
	
	f <- match.fun(f)

	missing(f) %throws% messages$function_is_required(func_call, "f")
	missing(x) %throws% messages$vector_is_required(func_call, "x")

	if (is.null(x)) return (NULL)
	if (length(x) == 0) return (integer(0))
	is.factor(x) %throws% messages$was_factor(func_call, x, "x")
	
	(!is_boolean(right)) %throws% 
		messages$not_a_bool(func_call, right, "right")
	
	cores <- get_cores(paropts)

	if (cores == 1) {
		if (right) {
			
			ind <- length(x)
			while (ind > 0) {
				if ( as.logical(f( x[[ind]] )) ) return (ind)
				ind <- ind - 1
			}
			return (integer(0))
			
		} else {
			
			ind <- 1
			while (ind <= length(x)) {

				if ( as.logical(f( x[[ind]] )) ) return (ind)
				ind <- ind + 1
			}
			return (integer(0))
		}
	}
 	
	job_indices <- if (right) {
		rev(group_into(seq_along(x), cores))
	} else {
		group_into(seq_along(x), cores) 
	}
	
	for (i in seq_along(job_indices)) {
		
		checked_ind <- unlist(call_mclapply(
			f = function (ind) {
				# returns indices satisfying f
				
				is_match <- as.logical(f( x[[ind]] ))	
				if (isTRUE(is_match)) ind else NaN
			},
			x = job_indices[[i]], paropts, func_call
		))
		
		matched_indices <- checked_ind[
			!is.nan(checked_ind) & checked_ind
		]
		
		if (length(matched_indices) > 0) {
			return (if (right) max(matched_indices) else min(matched_indices))
		}
	}
	integer(0)
}

#' @rdname mchof_positions
#' @family mchof-positions
#' @export

mcFind <- function (f, x, right = FALSE, paropts = NULL) {
	# returns the first (or last) element in x that matches
	# the predicate f

	func_call <- "mcFind(f, x, right = FALSE, paropts = NULL)"

	missing(f) %throws% messages$function_is_required(func_call, "f")
	missing(x) %throws% messages$vector_is_required(func_call, "x")
	
	if (length(x) == 0) return (x)
	is.factor(x) %throws% messages$was_factor(func_call, x, "x")
	
	first_match <- mcPosition (f, x, right, paropts)
	
	if (!is_integer0(first_match)) {
		x[[first_match]]
	} else integer(0)
}

