
library(mchof)
library(testthat)

### testing functions ###

assert <- function (info = '', rule, given = function (...) TRUE,
	unless = function (...) FALSE, where) {
	# check that an axiom holds	for a set of inputs
	
	check_case <- function (args) {
		# checks a single list of args
		
		if (do.call(given, args) && do.call(Negate(unless), args)) {
			fits_rule <- do.call(rule, args)
			
			if (!fits_rule) stop (info, ': a case did not meet the rule', args)
		}		
	}
	
	combos <- 
	
	return(TRUE)
}
Paropts <- function () {
	list(
		list(mc.cores = 1),
		list(mc.cores = 2),
		list(mc.cores = 5),
		list(mc.cores = 10)
	)
}
Booleans <- function () list(TRUE, FALSE)
LibFunctions <- function () {
	list(
		list(name = 'zip', f = mcZip),
		list(name = 'unzip', f = mcUnzip),
		list(name = 'reduce', f = mcReduce),
		list(name = 'position', f = mcPosition),
		list(name = 'partition', f = mcPartition),
		list(name = 'filter', f = mcFilter),
		list(name = 'find', f = mcFind)
	)
}
Functions <- function () {
	list(
		list(name = '', f = function (el) el )	
	)
}

test_dir('/home/rgrannell1/Dropbox/R directory/mchof/inst/tests/', rep ='stop')
