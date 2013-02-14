
context("make sure that the special cases for mchof are sensible")

test_that("mcFilter's special cases work like x[which(...)]", {
	
	x <- c(NA, NA)
	expect_equal(
		mcFilter(function(x) is.na(x), x),
		x[which(is.na(x))])
	
	expect_equal(
		mcFilter(function(x) !is.na(x), x),
		x[which(!is.na(x))])
	
	x <- c(NULL, NULL)
	expect_equal(
		mcFilter(
			function(x) is.null(x), x,
		), x[which(is.null(x))]
		
	x <- integer(0)
	expect_equal(
		mcFilter(
			function(x) T, x,
		), x)
})

test_that("mcPosition's special cases work like 
	x[which(...)[1]]", {
	
	x <- c(1,3,2,4,10)
	expect_equal(
		mcPosition(
			function(x) x > 3,	
			x),		
		x[which(x > 3)[1]])
	
})


















