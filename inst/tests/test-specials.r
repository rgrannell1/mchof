
context("make sure that the special cases for mchof are sensible")

test_that("mcFilter's special cases work like x[which(...)]", {
	
	expect_equal(
		mcFilter(function(x) is.na(x), c(NA, NA)),
		x[which(is.na(x))])
	
	expect_equal(
		mcFilter(function(x) !is.na(x), c(NA, NA)),
		x[which(!is.na(x))])
	
	expect_equal(
		mcFilter(
			function(x) is.null(x), NULL),
		NULL)
})