
mcPosition <- function(f, x, right=FALSE, nomatch, paropts = NULL){
	
	ncores <- if(!is.null(paropts) && 'mc.cores' %in% names(paropts)){
		paropts$mc.cores
	} else 1
	
	ind <- matrix(NA, 
		nrow = ncores,
		ncol = ceiling(length(x)/ncores))
	ind[seq_along(x)] <- seq_along(x)
	ind <- t(ind)
	
	print(ind)
	
	for(i in if(right) nrow(ind):1 else 1:nrow(ind)){
		
		res <- Reduce(
			f = rbind, 
			x = call_mclapply(
	   				function(j) c(j, f(x[[j]])), 
	   				ind[i,], paropts) )
		
		print(res)
		
		res <- res[,1] * res[,2]

		if(res != 0){
			return((ind[i,])[min(res > 0)])
		}
	}
	nomatch
}

mcPosition(function(x) x > 5, 1:10, paropts = list(mc.cores = 2))








