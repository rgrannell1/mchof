
get_cores <- function (paropts) {
	# check paropts and option(mc.cores) for the number 
	# of cores to use
	
	cores <- if (!is.null(paropts) && 'mc.cores' %in% names(paropts)) {
		abs(paropts$mc.cores)
	} else if (!is.null(getOption('mc.cores')))  {
		getOption('mc.cores')
	} else 1
	
	(!is.numeric(cores)) %throws% 
		messages$not_a_number(func_call, cores, "mc.cores")
	abs(round(cores, 0))
}

