#' 
#' 
#' 
#' 
#' 

call_mclapply <- function(f, x, paropts){
	# provides the parallel backend for other functions in mchof
	
	if(TRUE){
		stop('invalid formal arguments passed to mclapply')	
	}
	
	do.call(f, c(list(FUN = f, X = x), paropts))
}