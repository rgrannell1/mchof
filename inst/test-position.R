context("Check that mcPosition")

cat('   t-Position')

	cases <- 

	test_that("special cases work properly", {
		t_fun <- function(x) TRUE
		sapply(
			special_cases,	
			function(case){
				expect_equal(
					mcPosition(t_fun, case),	
					Position(t_fun, case))
			})
	})
	
	test_that("equal for random inputs", {
		
		replicate(10,{
			
			
			
			
		})
	})
	
	test_that("appropriate errors are thrown", {
		sapply(
			,	
			function(case){
				
		})
	})

context("Check that mcFind is well behaved")

cat('   t-Find')

	test_that("special cases work properly", {
		sapply(
			special_cases,	
			function(case){
				expect_equal(
					mcFind(),	
					Find())
			})
	})
	
	test_that("equal for random inputs", {
		
		replicate(10,{
			
			
			
			
		})
	})
	
	test_that("appropriate errors are thrown", {
		sapply(
			,	
			  function(case){
			  	
			  })
	})
	
