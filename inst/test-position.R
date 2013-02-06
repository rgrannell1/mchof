context("Check that mcPosition")

cat('   t-Position')

	test_that("special cases work properly", {
		sapply(
			cases,	
			function(case){
				expect_equal(
					mcPosition(),	
					Position())
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
			cases,	
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
	
