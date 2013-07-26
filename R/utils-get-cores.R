
get_cores <- function (paropts) {
	# check paropts and option(mc.cores) for the number 
	# of cores to use
	
	require_a(c("named list", "named paropts"), paropts)

	cores <- if (!is.null(paropts) && 'mc.cores' %in% names(paropts)) {
		abs(paropts$mc.cores)
	} else if (!is.null(getOption('mc.cores')))  {
		getOption('mc.cores')
	} else 1
	
	(!is.numeric(cores)) %throws% 
		messages$class_mismatch(func_call, cores, "mc.cores", "numeric")
	abs(round(cores, 0))
}

