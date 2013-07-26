
context("parallel backend checks")

if (exists('call_mclapply')) {
	
	test_that("errors are reported", {

		expect_error(
			mcSelect(
				function (x) {
					stop("do you see me?")
				},
				0, list(mc.cores = 2)
			) )
		expect_error(
			mcReduce(
				function (x, y) {
					stop("do you see me?")
				},
				list(1, 2, 3), list(mc.cores = 2)
			) )
		expect_error(
			mcZipWith(
				function (x, y) {
					stop("do you see me?")
				},
				list(1, 2, 3), paropts = list(mc.cores = 2)
		) )
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
