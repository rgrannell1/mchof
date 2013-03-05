
context("make sure factors throw errors, for now")

test_that("errors thrown by all functions", {

	true_fun <- function(...) TRUE
	test_factor <- factor(c(1,2,3,2,1))
	
	expect_error(
		mcReduce(true_fun, test_factor),	
		'factor')
	
	expect_error(
		mcFind(true_fun, test_factor),	
		'factor')
	
	expect_error(
		mcPosition(true_fun, test_factor),	
		'factor')
	
	expect_error(
		mcFilter(true_fun, test_factor),	
		'factor')	
})
