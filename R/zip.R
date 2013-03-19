
mcZip <- function (..., paropts = NULL) {
	# takes n lists/vectors, return a list of n-tuples. 
	# excess elements are discarded. 
	
	args <- lapply (as.list(match.call())[-1], eval)
	
	if (length(args) == 0) return (NULL)
	
	if (!is.null(names(args)) && 'paropts' %in% names(args)) {
		args$paropts <- NULL
	}
	
	shortest <- min(sapply(lists, length))
	
	if (shortest == 0) return (NULL)
	
	to_join <- lapply (
		lists, function (x) x[seq_len(shortest)] )
	
	call_mclapply (
		function (ind) {
			lapply (to_join, function (x) x[[ind]])
		},	
		seq_len(shortest), paropts)
}
