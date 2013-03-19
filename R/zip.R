
mcZipWith <- function (f, ..., paropts = NULL) {
	# takes n lists/vectors, generates a list of n-tuples. 
	# returns the result of mapping f over this new list. 
	# excess elements are discarded. 
	
	# list (x1, x2), list (y1, y2)  |-> list ( list(x1, y1), list(x2, y2) )
	
	args <- lapply (as.list(match.call())[-1], eval)
	
	if (length(args) == 0) return (NULL)
	
	if (!is.null(names(args))) {
		
		if ('paropts' %in% names(args)) args$paropts <- NULL
		if ('f' %in% names(args)) args$f <- NULL

	}
	
	shortest <- min(sapply(args, length))
	
	if (shortest == 0) return (NULL)
		
	to_zip <- lapply (
		args, function (x) x[seq_len(shortest)] )
	
	zipped <- call_mclapply (
		f = function (ind) {
			lapply (to_zip, function (x) x[[ind]])
		},	
		x = seq_len(shortest), 
		paropts)
	
	call_mclapply (
		f = f,	
		x = zipped,
		paropts )
}

mcZip <- function(..., paropts) mcZipWith (identity, ..., paropts)
