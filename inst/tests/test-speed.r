
context("ensure that there is some speedup from the package")

# aim for 60% speedup, outside of mcReduce which had odd complexity

test_that("find", {

	expect_that(
		mcFind(
			function (x) {
				Sys.sleep(0.5)
				FALSE
			},	
			x = seq_len(20),
			paropts = list(mc.cores = 2) ),		
			takes_less_than(6) )

})

test_that("position", {
	
	expect_that(
		mcPosition(
			function (x) {
				Sys.sleep(0.5)
				FALSE
			},	
			x = seq_len(20),
			paropts = list(mc.cores = 2) ),		
		takes_less_than(6) )
})

test_that("filter", {
	
	expect_that(
		mcFilter(
			function (x) {
				Sys.sleep(0.5)
				FALSE
			},	
			x = seq_len(20),
			paropts = list(mc.cores = 2) ),		
		takes_less_than(6) )
})
test_that("find", {
	
	expect_that(
		mcFind(
			function (x) {
				Sys.sleep(0.5)
				FALSE
			},	
			x = seq_len(20),
			paropts = list(mc.cores = 2) ),		
		takes_less_than(6) )
})
	
test_that("position", {
		
	expect_that(
		mcPosition(
			function (x) {
				Sys.sleep(0.5)
				FALSE
			},	
			x = seq_len(20),
			paropts = list(mc.cores = 2) ),		
		takes_less_than(6) )

})
	
test_that("reduce", {
		
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
	
### mchof 0.2 functions

test_that("zipwith", {
	
	expect_that(
		mcZipWith(
			function (x) {
				Sys.sleep(0.5)
				1				
			}, 	
			list(list(1:20), list(1:20)),
			paropts = list(mc.cores = 2) ),
		takes_less_than(6) )
	
})

test_that("mcpartition", {
	
	expect_that(
		mcPartition(
			function (n) {
				Sys.sleep(0.5)
				1
			},	
			1:20,
			paropts = list(mc.cores = 2)
		),	
		takes_less_than(6) )	
	
})



