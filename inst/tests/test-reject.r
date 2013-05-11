
context("test that reject is well-behaved")

test_that("normal tests for mcReject", {
	
	expect_equal(
		mcReject(
			function (x) (x %% 2 == 0),	
			1:10,
			paropts = list(mc.cores = 11)
		),
		c(1,3,5,7,9) )
	
	expect_equal(
		mcReject(
			function (x) unlist(x) == 2,	
			list( list(1), list(2), list(3) ),
			paropts = list(mc.cores = 11)
		),
		list(list(1), list(3)))
	
})