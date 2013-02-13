
context("parallel backend checks")

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
	
	
})

test_that("check that everything's running quick", {
	
	expect_that(
		system.time(call_mclapply(
			function(x) Sys.sleep(0.5), 1:10, list(mc.cores = 2)))[3],	
		takes_less_than(3))

})

