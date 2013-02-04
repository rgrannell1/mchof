#' a piece of boilerplate factored out of 
#' 

call_mclapply <- function(f, x, paropts = NULL){
	# provides the parallel backend for other functions in mchof
	
	if(!TRUE){
		stop('invalid formal arguments passed to mclapply')	
	}
	
	do.call(mclapply, c(list(FUN = f, X = x), paropts))
}