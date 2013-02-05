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
	
	replicate(100, {
	
		t_fun <- num_fun()[[1]]
		t_x <- x_gen(
			n=sample(10:100, size=1), 
			f = number_gen)
		t_paropts <- paropts_gen()[[1]]
		
		expect_equal(,
			mcFilter(
				f = t_fun,	
				x = t_x,
				paropts = t_paropts),	
			Filter(
				f = t_f,	
				x = t_x))
	})
})









