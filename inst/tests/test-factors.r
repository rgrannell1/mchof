
context("make sure factors throw errors")

true_fun <- function(...) TRUE

test_that("errors thrown by all functions", {

	expect_error(
		mcReduce(true_fun, factor(c(1,2,3,2,1)),	
		'factor')
	
	expect_error(
		mcFind(true_fun, factor(c(1,2,3,2,1))),	
		'factor')
	
	expect_error(
		mcPosition(true_fun, factor(c(1,2,3,2,1))),	
		'factor')
	
	expect_error(
		mcFilter(true_fun, factor(c(1,2,3,2,1))),	
		'factor')
	
})
