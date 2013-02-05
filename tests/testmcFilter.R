
context("this should run correctly")

test_that("", {
	
	mcFilter(function(x) x > 5, 1:10)
	
})

context("bad argument tests")

test_that("bad fun arguments", {
	expect_error(mcFilter(1234, 1:10)); expect_error(mcFilter(TRUE, 1:10))
	expect_error(mcFilter(NULL, 1:10)); expect_error(mcFilter(NA, 1:10)) })

test_that("bad x arguments", {
	expect_error(
		mcFilter(function(x) TRUE, factor(1)))
})

test_that("bad paropts arguments", {
	
	expect_error(
		mcFilter(function(x) x > 5, 1:10, paropts=list(FUN = mean)))	
	expect_error(
		mcFilter(function(x) x > 5, 1:10, list(f = mean)))
	expect_error(
		mcFilter(function(x) x > 5, 1:10, list(x = list(1:3))))
	expect_error(
		mcFilter(function(x) x > 5, 1:10, list(X = 10))	
	)
	expect_error(
		mcFilter(function(x) x > 5, 1:10, list(X = 10, FUN = 10)))
	expect_error(
		mcFilter(function(x) x > 5, 1:10, list(lol = 10, cat = 10)))
})

context("test equality with Filter")

test_that("same values of x return the same results for weird values", {
	
	expect_equal(
		mcFilter(function(y) y > 5, NA),
		Filter(function(y) y > 5, NA))
	expect_equal(
		mcFilter(function(y) y > 5, NULL),
		Filter(function(y) y > 5, NULL))
	expect_equal(
		mcFilter(function(y) y > 5, NaN),
		Filter(function(y) y > 5, NaN))
	expect_equal(
		mcFilter(function(y) y > 5, Inf),
		Filter(function(y) y > 5, Inf))
	expect_equal(
		mcFilter(function(y) y > 5, list()),
		Filter(function(y) y > 5, list()))
	expect_equal(
		mcFilter(function(y) y > 5, list(list())),
		Filter(function(y) y > 5, list(list())))
	
})

test_that("same values of x return same results (normal vals)", {
	
	for(i in 1:40){
		
		random_data <- sample(1:100, size = sample(1:10))
		expect_equal(
			mcFilter(function(y) y > 5, random_data),
			Filter(function(y) y > 5, random_data))	
	}
})

