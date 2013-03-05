
context("test that filter is well-behaved")
	
test_that("normal tests for filter", {

	expect_equal(
		mcFilter(
			function (x) (x %% 2 == 0),	
			1:10,
			paropts = list(mc.cores = 11)
		),
		c(2,4,6,8,10) )
	
	expect_equal(
		mcFilter(
			function (x) FALSE,	
			1:10,
			paropts = list(mc.cores = 6)
		),
		numeric(0))
	  
	expect_equal(
		mcFilter(
			function (x) unlist(x) == 2,	
			list( list(1), list(2), list(3) ),
			paropts = list(mc.cores = 11)
		),
		list(list(2)))
	
})
	
	
	
	








