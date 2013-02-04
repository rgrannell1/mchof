#' a piece of boilerplate factored out of 
#' 

call_mclapply <- function(f, x, paropts = NULL){
	# provides the interface to the parallel backend for 
	# other functions in mchof
	
	arg_names <- names(paropts)
	invalid_args <- setdiff(arg_names, formals(mclapply))
	
	if(length(invalid_args) > 0){
		
		stop(paste(
			invalid_args,
			"are not formal arguments for mclapply"))
		
	}
	if('f' && 'FUN' %in% arg_names ||
	   'FUN' && 'FUN' %in% arg_names ||   	
	   'f' && 'f' %in% arg_names) stop("f/FUN was matched multiple times")
	
	if('x' && 'X' %in% arg_names ||
	   	'X' && 'X' %in% arg_names ||   	
	   	'x' && 'x' %in% arg_names) stop("x/X was matched multiple times")

	do.call(mclapply, c(list(FUN = f, X = x), paropts))
}