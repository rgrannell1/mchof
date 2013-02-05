context("ensures mcFilter is well behaved")

cat('  t-filter')


test_that("special cases work properly", {
	fun <- function(x) TRUE
	cases <- list(
		NULL, NA, NaN, Inf, -Inf, NA_integer_, 	
		NA_real_, NA_character_, NA_complex_, integer(0),
		list(), list(list()))

	sapply(
		cases,
		function(case){
			expect_equal(info=case,
				mcFilter(fun, case, paropts=list(mc.cores = 2)),	
				Filter(fun, case))
	})
})

test_that("equal results for random inputs", {
	
	mcFilter(
		f = num_fun(),	
		x = x_gen(100, number_gen()),
		paropts = paropts_gen())	
	
})









