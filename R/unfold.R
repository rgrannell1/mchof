#' @title mcUnfold
#' 
#' @export
#' @description mcUnfold
#' 
#' @details mcUnfold is potentially non-terminating; caution must be taken to
#' terminate the unfold.
#' 
#' @name mcUnfold
#' 
#' @param f a function that generates a list
#' @param first an initial value for the first position of f
#' @param paropts paropts a list of parameters to be handed to 
#'    mclapply (see details and \code{\link{mclapply}})
#' 
#' @example 
#' 
#' # enumerate the integers 1...10
#' mcUnfold (
#'     function (x) if (x < 10) c(x, x+1),
#'     list(1)
#' )
#' 
#' 

mcUnfold <- function (f, x, paropts = NULL) {
	# list anamorphism. from an initial seed value, produce
	# a potentially infinite amount of values. 
	# may be non-terminating

	is_empty_list <- function (x) {
		is.null(x) ||
			(is.list(x) && length(x) == 0)
	}
	
	f <- match.fun(f)
	
	if (is.null(x)) return (NULL)
	if (is.list(x) && length(x) == 0) return(list())
	if (is.factor(x)) stop('x may not be a factor')	
	
	unfoldable <- c(x)

	len <- function () {
		length(unfoldable)
	}

	repeat {
		res <- f( unfoldable[[ len() ]] )
		
		if (is_empty_list(res)) {
			return (unfoldable)
		} else {
			unfoldable <- c(head(unfoldable, -1), res)
		}
	}
	
	unfoldable
}

