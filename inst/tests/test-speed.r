
context("ensure that there is some speedup from the package")

# aim for 60% speedup, outside of mcReduce which had odd complexity

test_that("find", {

	expect_that(
		mcFind(function (x) {
			Sys.sleep(0.5)
			FALSE
		},	
		x = seq_len(20), paropts = list(mc.cores = 2) ),		
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
test_that("reject", {
	
	expect_that(
		mcReject(
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
	
test_that("fold", {
	
	expect_that(
		mcFold(
			function (x, y) {
				Sys.sleep(0.5)
				1
			},	
			0,
			x = seq_len(19),
			paropts = list(mc.cores = 2) ),		
		takes_less_than(6) )
	
})

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

test_that("quantifiers", {

	expect_that(
		mcAll(
			function (x) {
				Sys.sleep(0.5)
				TRUE
			}, 1:20
		), takes_less_than(6)
	)

	expect_that(
		mcAny(
			function (x) {
				Sys.sleep(0.5)
				TRUE
			}, 1:20
		), takes_less_than(6)
	)
	
	expect_that(
		mcOne(
			function (x) {
				Sys.sleep(0.5)
				TRUE
			}, 1:20
		), takes_less_than(6)
	)
	
})
