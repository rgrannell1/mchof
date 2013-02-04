#
#

call_mclapply <- function(f, x, ordered = NULL, paropts = NULL){
	# provides the interface to the parallel backend for 
	# other functions in mchof

	arg_names <- names(paropts)
	invalid_args <- setdiff(arg_names, formals(mclapply))
	
	if(length(invalid_args) > 0){	
		stop(invalid_args, " are not formal arguments for mclapply")
	}
	if(any(c('f', 'FUN') %in% paropts)){
		stop('f or FUN may not be specified in paropts')
	}
	if(any(c('x', 'X') %in% paropts)){
		stop('x or X may not be specified in paropts')
	}
	
	
	do.call(mclapply, c(list(FUN = f, X = x), paropts))
}

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
#-#-# UNIT TESTS FOR CALL_MCLAPPLY
#  
# likely bug: order not preserved.
# 
#-----------------------------#

require(testthat); require(multicore)

call_mclapply()








