
context("special length cases of x handled consistently")

true_fun <- function(x) TRUE

test_that("mcPosition length(0) |-> length(0)", {
	
	# [A](0) |-> integer(0)
	expect_equal(	
		mcPosition(
			true_fun, x = character(0)),
		integer(0))
	
	# NULL |-> integer(0)
	expect_equal(	
		mcPosition(
			true_fun, x = integer(0)),
		integer(0))	
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

test_that("mcFilter length(0) |-> length(0)", {
	
	# NULL |-> NULL
	expect_equal(
		mcFilter(
			function(x) T,
			NULL
		), NULL)
	
	# [A](0) |-> [A](0)
	expect_equal(
		mcFilter(
			function(x) T,
			integer(0)
		), integer(0))
})

test_that("mcReduce length(0) |-> length(0) & length(1) |-> 1", {

	# len(x) == 1 |-> 1
	expect_equal(
		mcReduce(get('+'), c(1)), 
		1)
	
	# NULL |-> NULL 
	expect_equal(
		mcReduce(get('+'), NULL),
		NULL)
	
})
