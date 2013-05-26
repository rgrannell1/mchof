
context("make sure factors throw errors, for now")

### mchof 0.1 functions ###
#-#-#-#-#-#-#-#-#-#-#-#-#-#

test_that("errors thrown by all functions", {

	true_fun <- function(...) TRUE
	test_factor <- factor(c(1,2,3,2,1))
	
	expect_error(mcReduce(true_fun, test_factor), 'factor')
	
	expect_error(mcFind(true_fun, test_factor), 'factor')
	
	expect_error(mcPosition(true_fun, test_factor), 'factor')
	
	expect_error(mcFilter(true_fun, test_factor), 'factor')	
	
	### mchof 0.2 functions ###
	#-#-#-#-#-#-#-#-#-#-#-#-#-#
	
	expect_error(mcZipWith(truefun, list(test_factor, test_factor)), 'factor')
	
	expect_error(mcZip(list(test_factor, test_factor)), 'factor')
	
	expect_error(mcUnzip(list(test_factor)), 'factor')
	
	expect_error(mcUnzipWith(truefun, list(test_factor)), 'factor')
	
	expect_error(mcPartition(true_fun, test_factor), 'factor')		
	
	### mchof 0.3 functions ###
	#-#-#-#-#-#-#-#-#-#-#-#-#-#
	
	expect_error(mcAll(true_fun, test_factor), 'factor')
	
	expect_error(mcAny(true_fun, test_factor), 'factor')

	expect_error(mcOne(true_fun, test_factor), 'factor')
	
	expect_error(mcReject(true_fun, test_factor), 'factor')

	expect_error(mcFold(true_fun, 0, test_factor), 'factor')	
	
}
		  