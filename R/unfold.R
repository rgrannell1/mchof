#' @title mcUnfold
#' 
#' @export
#' @description mcUnfold
#' 
#' @details mcUnfold is dual to mcFold. 
#' 
#' @name mcUnfold
#' 
#' @param f a function that generates a list
#' @param first an initial value for the first position of f
#' @param paropts paropts a list of parameters to be handed to 
#'    mclapply (see details and \code{\link{mclapply}})
#' 

# ana f x = case (f x) of
# 	Nothing -> []
# 	Just (a,y) ->  a:(ana f y)

mcUnfold <- function (f, x, paropts = NULL) {
	# list anamorphism. from an initial seed value, producing
	# a potentially infinite amount of values. 
	
	f <- match.fun(f)
			
	
	
	

}












