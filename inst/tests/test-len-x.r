
context("special cases of x handled consistently")

true_fun <- function(x) TRUE

test_that("mcPosition length(0) |-> length(0)", {
	
	# integer(0) |-> integer(0)
	expect_equal(	
		mcPosition(
			true_fun, x = integer(0)),
		integer(0))
	
	# NULL |-> integer(0)
	expect_equal(	
		mcPosition(
			true_fun, x = integer(0)),
		NULL)	
})

test_that("mcFind length(0) |-> length(0)", {

	expect_equal(
		length(mcFind(
			function(x) FALSE, 
			character(0)
		)),	0)
	
	# mcFind NULL |-> NULL
	expect_equal(
		mcFind(true_fun, NULL),	
		NULL)
	
})
