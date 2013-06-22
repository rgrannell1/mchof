
get_cores <- function (paropts) {
	# check paropts and option(mc.cores) for the number 
	# of cores to use
	
	cores <- if (!is.null(paropts) && 'mc.cores' %in% names(paropts)) {
		abs(paropts$mc.cores)
	} else if (!is.null(getOption('mc.cores')))  {
		getOption('mc.cores')
	} else 1
	
	(!is.numeric(cores)) %throws% 
		messages$wrong_class(func_call, cores, "numeric", "mc.cores")
	abs(round(cores, 0))
}

