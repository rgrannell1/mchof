
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

}

Functions <- list(
	list(name = 'zip', mcZip),
	list(name = 'unzip', mcUnzip),
	list(name = 'reduce', mcReduce),
	list(name = 'position', mcPosition),
	list(name = 'partition', mcPartition),
	list(name = 'filter', mcFilter),
	list(name = 'find', mcFind)
)

test_dir('/home/rgrannell1/Dropbox/R directory/mchof/inst/tests/', rep ='stop')
