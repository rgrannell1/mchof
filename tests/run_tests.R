
library(mchof)
library(testthat)

##=-=-=- ASSERTION FUNCTION AND TEST-CASE GENERATORS -=-=-=-=-=-=-=-=-=-=-=-=-=#
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=

assert <- function (info = '', always, where, unless = function (...) FALSE) {
	# check that a condition is true over a range of value, unless...
	
	combos <- do.call (
		expand.grid,
		Map (seq_len, Map (length, where)))

	unlist(apply (unname(combos), 1,
		function (ind) {
			
			args <- Map ( 
				function (i, x) where [[i]] [[x]],
				 seq_along(ind), ind)
			
			expect_true (info = info, 
				if (!do.call(unless, args)) do.call(always, args) else TRUE)
	}))
}

Collection <- function (n) {
	# unconstrained collection generators
	
}
List <- function (n) {
	# returns a list of lists, including the empty
	# list and non-flatlists
}
Vector <- function (n) {
	# returns a list of vectors of many types
	
}

###=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
###=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
###=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=

test_dir('/home/rgrannell1/Dropbox/R directory/mchof/inst/tests/', rep ='stop')
