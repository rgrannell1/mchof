#' @export

check_paropts <- function(paropts){
	# stops the program if invalid arguments are
	# included in paropts
	
	arg_names <- names(paropts)
	invalid_args <- setdiff(arg_names, names(formals(mclapply)))
	
	if(length(invalid_args) > 0){
		
		notin <- if(length(invalid_args) > 1){
			" aren't in"
		} else {
			" isn't in"
		}
		stop("parameters ",
			paste(invalid_args, collapse = ','),
			" in paropts", notin,
			" the formal arguments for mclapply")
	}
	if(any(c('f', 'FUN') %in% names(paropts))){
		stop('f or FUN may not be specified in paropts')
	}
	if(any(c('x', 'X') %in% names(paropts))){
		stop('x or X may not be specified in paropts')
	}
}

call_mclapply <- function(f, x, paropts = NULL){
	# provides the interface to the parallel backend for 
	# other functions in mchof
	
	if(!is.function(f)) stop('f is not a function')
	
	if(!is.null(paropts)){
		check_paropts(paropts)
	}
	do.call(mclapply, c(list(FUN = f, X = x), paropts))
}
