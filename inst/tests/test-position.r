
context("normal case testing for position")

test_that("test that the answer is invariant under mc.cores and right", {
	expect_equal(
		mcPosition(
			function(x) x,	
			c(F, F, T, F, F, T, F) ), 3)
	
	expect_equal(
		mcPosition(
			function(x) x,
			c(F, F, T, F, F, T, F), right = TRUE), 6)
	
	expect_equal(
		mcPosition(
			function(x) x,	
			c(F, F, T, F, F, T, F), paropts = list(mc.cores = 4)), 3)
	
	expect_equal(
		mcPosition(
			function(x) x,
			c(F, F, T, F, F, T, F),
			right = TRUE,
			list(mc.cores = 12)), 6)
	
	expect_equal(
		mcPosition(
			function(x) x > 5,
			c(1, 3, 5, 6, 7),
			right = TRUE,
			list(mc.cores = 4)), 5)
	
})
