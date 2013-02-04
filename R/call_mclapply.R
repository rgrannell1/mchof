#' a piece of boilerplate factored out of 
#' 

call_mclapply <- function(f, x, paropts = NULL){
	# provides the interface to the parallel backend for 
	# other functions in mchof
	
	invalid_args <- setdiff(names(paropts), formals(mclapply))
	
	if(length(invalid_args) > 0){
		
		stop(paste(
			invalid_args,
			"are not formal arguments for mclapply"))
	}
	#'#' Multiple FUN, f, x, X, ..., arguments

	do.call(mclapply, c(list(FUN = f, X = x), paropts))
}