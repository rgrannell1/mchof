context("ensures mcFilter is well behaved")
	
	cat('  t-filter')
	
	cases <- list(
		NULL, NA, NaN, Inf, -Inf, NA_integer_, 	
		NA_real_, NA_character_, NA_complex_, integer(0),
		list(), list(list()))
	
	test_that("special cases work properly", {
		fun <- function(x) TRUE
	
		sapply(
			cases,
			function(case){
				expect_equal(
					mcFilter(fun, case, paropts=list(mc.cores = 2)),	
					Filter(fun, case))
		})
	})
	
	test_that("equal results for random inputs", {
		
		replicate(10, {
		
			t_fun <- num_fun()[[1]]
			t_x <- x_gen(
				n=sample(10:100, size=1), 
				f = number_gen)
			t_paropts <- paropts_gen()[[1]]
			
			expect_equal(label='Filter',
				mcFilter(
					f = t_fun,	
					x = t_x,
					paropts = t_paropts),	
				Filter(
					f = t_fun,	
					x = t_x)) })
	})
	
	test_that("the appropriate errors are thrown", {
	
		sapply(
			cases,	
			function(f){
				expect_error(
					mcFilter(
						f = f,	
						x = x_gen(
							n=sample(10:100, size=1), 
							f = number_gen),
						paropts = paropts_gen()[[1]]
					))
			})
		
		sapply(
			list(function(crash) 'Crash!'),	
			function(x){
				expect_error(
					mcFilter(
						f = function(y) TRUE,
						x = x,
						paropts = paropts_gen()[[1]] 
					))
				expect_error(
					Filter(
						f = function(y) TRUE,		
						x = x
					))
		})	
		
	})
