#
#

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

	do.call(mclapply, c(list(FUN = f, X = x), paropts))
}

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
#-#-# UNIT TESTS FOR CALL_MCLAPPLY
#  
# 
# 
#-----------------------------#

require(testthat); require(multicore)

replicate(
	100,
	
)










