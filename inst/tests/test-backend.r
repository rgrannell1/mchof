
context("parallel backend checks")

if (exists('call_mclapply')) {
	
	test_that("errors are reported", {
		
		suppressWarnings ({
		
			errfunc_1 <- function (x) stop('do you see me?') 
			errfunc_2 <- function (x, y) stop('do you see me, monoid?') 
			
			expect_error(mcReject(errfunc_1, 1:10, list(mc.cores = 4)), 'see me?')
			expect_error(mcFilter(errfunc_1, 1:10, list(mc.cores = 4)), 'see me?')
			expect_error(mcPartition(errfunc_1, 1:10), 'see me?')
			
			expect_error(mcPosition(errfunc_1, 1:10), 'see me?')
			expect_error(mcFind(errfunc_1, 1:10), 'see me?')
			expect_error(mcAll(errfunc_1, 1:10), 'see me?')
			expect_error(mcAny(errfunc_1, 1:10), 'see me?')
			expect_error(mcOne(errfunc_1, 1:10), 'see me?')

			expect_error(mcReduce(errfunc_2, 1:10), 'see me?')
			expect_error(mcFold(errfunc_2, 0,1:10), 'see me?')
		
			expect_error(mcUnzipWith(squash(errfunc_1), 1:10), 'see me?')	
			expect_error(mcZipWith(squash(errfunc_1), 1:10), 'see me?')
			
		})
		
	})

	forall(
		info = "check call_mclapply is returning correct results",
		list(x_ = r_seq_len(), paropts_ = r_paropts()),
		function (x_, paropts_) {
			res <- call_mclapply(function (x) sum(unlist(x)), list(x_), paropts_)
			unlist(res) == sum(unlist(x_))
		}
	)
	
	if (.Platform$OS.type == 'unix') {
		
		test_that("error when given bad arguments", {
		    expect_error(
		    	call_mclapply("cat", 1:10, list(mc.cores = 2)),
		    	regexp = "f is not")

		    expect_error(
		    	call_mclapply(function(x) x, 1:10, list(monkey = identity)),
		    	regexp = "invalid")
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
			
			options(mc.cores = NULL)
		})
	}
}
