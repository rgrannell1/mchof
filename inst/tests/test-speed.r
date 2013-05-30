
context("ensure that there is roughly x2 speedup from the package for expensive cases")

# aim for 60% speedup, outside of mcReduce which had odd complexity

slow_truefunc <- function (...) {
	Sys.sleep(0.5)
	TRUE
}
slow_falsefunc <- Negate(slow_truefunc)

test_that("find", {

	expect_that(
		mcFind(slow_falsefunc, seq_len(20), list(mc.cores = 2)),		
		takes_less_than(6))

})

test_that("position", {
	
	expect_that(
		mcPosition(slow_falsefunc,	seq_len(20), list(mc.cores = 2)),		
		takes_less_than(6) )
})

test_that("filter", {
	
	expect_that(
		mcFilter(slow_falsefunc, seq_len(20), list(mc.cores = 2)),		
		takes_less_than(6) )
})
test_that("reject", {
	
	expect_that(
		mcReject(slow_falsefunc, seq_len(20), list(mc.cores = 2)),		
		takes_less_than(6) )
})

test_that("find", {
	
	expect_that(
		mcFind(slow_falsefunc, seq_len(20), list(mc.cores = 2)),		
		takes_less_than(6) )
})
	
test_that("position", {
		
	expect_that(
		mcPosition(slow_falsefunc, seq_len(20), list(mc.cores = 2)),		
		takes_less_than(6))

})
	
test_that("reduce", {
		
	expect_that(
		mcReduce(slow_truefunc, seq_len(20), list(mc.cores = 2)),		
		takes_less_than(6) )

})
	
test_that("fold", {
	
	expect_that(
		mcFold(slow_truefunc, 0, seq_len(19), list(mc.cores = 2) ),		
		takes_less_than(6) )
	
})

test_that("zipwith", {
	
	expect_that(
		mcZipWith(slow_truefunc, list(list(1:20), list(1:20)), list(mc.cores = 2)),
		takes_less_than(6) )
	
})

test_that("mcpartition", {
	
	expect_that(
		mcPartition(slow_truefunc, 1:20, list(mc.cores = 2)),	
		takes_less_than(6) )	
	
})

test_that("quantifiers", {

	expect_that(
		mcAll(slow_truefunc, 1:20, list(mc.cores = 2)), takes_less_than(6)
	)

	expect_that(
		mcAny(slow_falsefunc, 1:20, list(mc.cores = 2)), takes_less_than(6)
	)
	
	expect_that(
		mcOne(slow_truefunc, 1:20, list(mc.cores = 2)), takes_less_than(6)
	)
	
})
