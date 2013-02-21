
context("make sure factors aren't handled where they shouldn't be")

test_that("mcFilter", {

	expect_error(
		mcFilter(),	
		'')
	
})

test_that("mcPosition", {
	
	expect_error(
		mcPosition(),	
		'')
	
})

test_that("mcFind", {
	
	expect_error(
		mcFind(),	
		'')
	
})

test_that("Reduce", {
	
	expect_error(
		mcReduce(),	
		'')
	
})
