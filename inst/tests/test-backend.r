context("parallel backend checks")

if (exists('call_mclapply')) {
	
	test_that("errors are reported", {
		
		suppressWarnings ({
		
			errfunc_1 <- function (x) stop('do you see me?') 
			errfunc_2 <- function (x, y) stop('do you see me, monoid?') 
			
			expect_error(mcFilter(errfunc_1, 1:10, list(mc.cores = 4)), 'see me?')
			expect_error(mcPartition(errfunc_1, 1:10), 'see me?')
			
			expect_error(mcPosition(errfunc_1, 1:10), 'see me?')
			expect_error(mcFind(errfunc_1, 1:10), 'see me?')
			
			expect_error(mcReduce(errfunc_2, 1:10), 'see me?')
		
			expect_error(mcUnzipWith(errfunc_1, 1:10), 'see me?')	
			expect_error(mcZipWith(errfunc_1, 1:10), 'see me?')
			
		})
		
	})
	
	test_that("error when given bad arguments", {
	    expect_error(
	    	call_mclapply("cat", 1:10, list(mc.cores = 2)),
	    	regexp = "f is not")
	    
	    expect_error(
	    	call_mclapply(function(x) x, factor(1), list(mc.cores = 2)),
	    	regexp = "x is not")
	    
	    expect_error(
	    	call_mclapply(function(x) x, 1:10, list(FUN = identity)),
	    	regexp = "FUN may not")
	    
	    expect_error(
	    	call_mclapply(function(x) x, 1:10, list(monkey = identity)),
	    	regexp = "invalid")
	})
	
	test_that("returns correct results for sample cases", {
		expect_equal(
			call_mclapply(function(x) x, 1:5),
			list(1,2,3,4,5))
		
		expect_equal(
			call_mclapply(function(x) !is.na(x),
			c(1, 2, 3, NA, 4, 5)), list(TRUE, TRUE, TRUE, FALSE, TRUE, TRUE))
	
	})
	
	test_that("check that everything's running quick", {
		
		expect_that(
			system.time(call_mclapply(
				function(x) Sys.sleep(0.5), 1:10, list(mc.cores = 2)))[3],	
			takes_less_than(3)) # at least 60% speedup
	
	})
	
	test_that("check that the options mechanism works for parallel backend", {
		options(mc.cores = 2)
		
		expect_that(
			system.time(call_mclapply(
				function(x) Sys.sleep(0.5), 1:10))[3],	
			takes_less_than(3)) # at least 60% speedup
		
	})
}