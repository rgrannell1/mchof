
mcPosition <- function(f, x, right, nomatch, paropts = NULL){
	
	ncores <- if(!is.null(paropts) && 'mc.cores' %in% names(paropts)){
		paropts$mc.cores
	} else 1
	
	ind <- matrix(NA, 
		nrow = ncores,
		ncol = ceiling(length(x)/ncores))
	ind[seq_along(x)] <- seq_along(x)
	
	for(i in if(right) ncores:1 else 1:ncores){
		
		res <- Reduce(
			f = function(e1, e2) rbind(e1, e2), 
			x = call_mclapply(
	   				function(j) c(j, x[[j]]), 
	   				ind[i,], paropts) )
		res <- res[,1] * res[,2]
		
		if(any(res != 0)){
			return((ind[i,])[min(res > 0)]
		}
	}
	nomatch
}



