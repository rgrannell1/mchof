
get_cores <- function (paropts) {
	# check paropts and option(mc.cores) for the number 
	# of cores to use
	
	pcall <- sys.call()
	require_a(c("named list", "named pairlist"), paropts, pcall)

	cores <- if (!is.null(paropts) && 'mc.cores' %in% names(paropts)) {
		paropts$mc.cores
	} else if (!is.null(getOption('mc.cores')))  {
		getOption('mc.cores')
	} else 1
	
	require_a("length-one positive whole", cores, pcall)

	cores
}

