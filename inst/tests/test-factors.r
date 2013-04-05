
context("make sure factors throw errors, for now")

### mchof 0.1 functions ###
#-#-#-#-#-#-#-#-#-#-#-#-#-#

test_that("errors thrown by all functions", {

	true_fun <- function(...) TRUE
	test_factor <- factor(c(1,2,3,2,1))
	
	expect_error(
		mcReduce(true_fun, test_factor), 'factor')
	
	expect_error(
		mcFind(true_fun, test_factor), 'factor')
	
	expect_error(
		mcPosition(true_fun, test_factor), 'factor')
	
	expect_error(
		mcFilter(true_fun, test_factor), 'factor')	
	
	### mchof 0.2 functions ###
	#-#-#-#-#-#-#-#-#-#-#-#-#-#
	
	expect_error(
		mcZipWith(
			function (...) TRUE,
			list(factor(1:10), factor(11:20))
		), 'factor')
	
	expect_error(
		mcZip(
			list(factor(1:4), factor(5:8))
		), 'factor')
	
	expect_error(
		mcUnzip(
			list(factor(1:5))	
		), 'factor')
	
	expect_error(
		mcUnzipWith(
			list(factor(1:5))	
		), 'factor')
	
	expect_error(
		mcPartition(true_fun, test_factor),	'factor')		
})
