
context("ensure that there is some speedup from the package")

# aim for 60% speedup, outside of mcReduce

test_that("speed tests", {

	expect_that(
		mcFind(
			function (x) {
				Sys.sleep(0.5)
				FALSE
			},	
			x = seq_len(20),
			paropts = list(mc.cores = 2) ),		
			takes_less_than(6) )
		
	expect_that(
		mcPosition(
			function (x) {
				Sys.sleep(0.5)
				FALSE
			},	
			x = seq_len(20),
			paropts = list(mc.cores = 2) ),		
		takes_less_than(6) )
		
	
	expect_that(
		mcFilter(
			function (x) {
				Sys.sleep(0.5)
				FALSE
			},	
			x = seq_len(20),
			paropts = list(mc.cores = 2) ),		
		takes_less_than(6) )
	
	
	expect_that(
		mcFind(
			function (x) {
				Sys.sleep(0.5)
				FALSE
			},	
			x = seq_len(20),
			paropts = list(mc.cores = 2) ),		
		takes_less_than(6) )
	
	expect_that(
		mcPosition(
			function (x) {
				Sys.sleep(0.5)
				FALSE
			},	
			x = seq_len(20),
			paropts = list(mc.cores = 2) ),		
		takes_less_than(6) )
	
	
	expect_that(
		mcReduce(
			function (x, y) {
				Sys.sleep(0.5)
				1
			},	
			x = seq_len(20),
			paropts = list(mc.cores = 2) ),		
		takes_less_than(6) )
		

	
	
})