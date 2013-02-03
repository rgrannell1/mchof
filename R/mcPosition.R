
mcPosition <- function(f, x, right, nomatch, paropts = NULL){
	
	ncores <- if(!is.null(paropts) && 'mc.cores' %in% names(paropts)){
		paropts$mc.cores
	} else 1
	
	ind <- matrix(NA, 
		nrow = ncores,
		ncol = ceiling(length(x)/ncores))
	ind[seq_along(x)] <- x

	for(){
		
	}
}



