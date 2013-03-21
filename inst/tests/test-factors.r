
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
	
	### mchof 0.2 functions
	
	expect_error(
		mcZipWith(
			function (...) TRUE,
			factor(1:10),
			factor(11:20)
		),
		'factor')
	
	expect_error(
		mcZip(
			factor(1:4),
			factor(5:8)
		),
		'factor')
	
	expect_error(
		mcUnzip(
			
		),
		'factor')
	
	expect_error(
		mcPartition(true_fun, test_factor),	
		'factor')	
	
})






