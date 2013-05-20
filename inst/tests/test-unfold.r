
context("unfold is well behaved")

test_that("normal cases of mcUnfold work", {
	
	expect_equal(
		mcUnfold(
			f = function (n) {
				if (n < 4) list(n, n + 1)		
			},
			x = list(0)
		),
		list(1,2,3,4,5))
	
	expect_equal(
		mcUnfold(
			f = function (n) {
				if (n < 32) list(n, 2 * n)		
			},
			x = c(2),
			paropts = list(mc.cores = 2)
		),
		list(2,4,8,16,32))

})
