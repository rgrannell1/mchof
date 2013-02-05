#

call_mclapply <- function(f, x, paropts = NULL){
	# provides the interface to the parallel backend for 
	# other functions in mchof
	
	if(!is.function(f)) stop('f is not a function')
	if(!is.vector(x)) stop('x is not a vector/list')
	
	if(!is.null(paropts)){

		arg_names <- names(paropts)
		invalid_args <- setdiff(arg_names, names(formals(mclapply)))
			
		if(length(invalid_args) > 0){
			
			not <- if(length(invalid_args) > 1) " aren't in" else " isn't in"
			
			stop(
				"parameters ",
				paste(invalid_args, collapse = ','),
				" in paropts", not," the formal arguments for mclapply")
		}
		if(any(c('f', 'FUN') %in% names(paropts))){
			stop('f or FUN may not be specified in paropts')
		}
		if(any(c('x', 'X') %in% names(paropts))){
			stop('x or X may not be specified in paropts')
		}	
	}

	do.call(mclapply, c(list(FUN = f, X = x), paropts))
}
