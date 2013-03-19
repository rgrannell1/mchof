
mcZip <- function (..., paropts = NULL) {
	# takes n lists/vectors, return a list of n-tuples. 
	# excess elements are discarded. 
	
	lists <- lapply (as.list(match.call())[-1], eval)
	
	if (!is.null(names(lists)) && 'paropts' %in% names(lists)) {
		lists$paropts <- NULL
	}
	if (length(lists) == 0) return(NULL)
	
	shortest <- min(sapply(lists, length))


	
	
	
	return(lists)
}
d <- mcZip (
	c(1,2,3),
	c(2,3,4,5),
	paropts = list(mc.cores = 2)
)

