
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
		which(x > 3)[1]
		)
	
	expect_equal(
		mcPosition(
			is.null,
			NULL), 1)
	
	x <- c(1,2,3,NA,5,6)
	expect_equal(
		mcPosition(is.na, x),	
		which(is.na(x))[1])
	
	expect_equal(
		mcPosition(
			function(x) x > 3,	
			x, right=TRUE),		
		which(rev(x) > 3)[1])
	
	expect_equal(
		mcPosition(
			function(x) x > 3,	
			x),	
		which(x > 3)[1], paropts = list(mc.cores = 2)
	)	
})

test_that("mcFind returns the correct value", {
	
	
})





