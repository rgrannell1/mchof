
mcPosition <- function(f, x, right=FALSE, nomatch=NA, paropts = NULL){
	
	ncores <- if(!is.null(paropts) && 'mc.cores' %in% names(paropts)){
		paropts$mc.cores
	} else 1
	
	ind <- matrix(NA, 
		nrow = ncores,
		ncol = ceiling(length(x)/ncores))
	ind[seq_along(x)] <- seq_along(x)
	ind <- if(right) t(apply(ind, 2,rev)) else t(ind)
		
	for(i in if(right) nrow(ind):1 else 1:nrow(ind)){
		
		res <- Reduce(
			f = rbind, 
			x = call_mclapply(
	   				function(j) c(j, f(x[[j]])), 
	   				ind[i,], paropts) )
			
		res <- res[,1] * res[,2]

		if(any(res[!is.na(res)] != 0)){
			return((ind[i,])[min(which(res > 0))])
		}
	}
	nomatch
}

system.time(
mcPosition(
	function(x){Sys.sleep(0.3); F}, 1:100,
	right = F,
	paropts = list(mc.cores = 4))
)

val <- c()




