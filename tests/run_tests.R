
library(mchof)
library(testthat)

##=-=-=- ASSERTION FUNCTION AND TEST-CASE GENERATORS -=-=-=-=-=-=-=-=-=-=-=-=-=#
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=

assert <- function (info = '', always, where, unless = function (...) FALSE) {
	# check that a condition is true over a range of value, so long as the
	# unless predicate isn't met
	
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
	
	replicate (n, ( function () {
		
		sample (
			list (
				list(),
				seq_len(100), as.list(seq_len(100)),
				paste0(seq_len(100)), as.list(paste0(seq_len(100)))

			), size = 1)
		
	} )(), simplify = FALSE)
	
}

List <- function (n) {
	# returns lists (some nested), and the empty list
	
	Filter( function (e) is.list(e), Collection(n))
}

Vector <- function (n) {
	# returns only vector elements (length > 1)
	
	Filter( function (e) is.vector(e), Collection(n))
}

###=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
###=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
###=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=

test_dir('/home/rgrannell1/Dropbox/R directory/mchof/inst/tests/', rep ='stop')
