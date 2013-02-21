
context("special cases of x handled consistently")

true_fun <- function(x) TRUE

test_that("mcPosition ~ which(x == ...)", {
	
	# integer(0) |-> integer(0)
	expect_equal(	
		mcPosition(
			true_fun, x = integer(0)),
		integer(0) )
	
	# NULL |-> 
	
	# NA |-> 
	
})
