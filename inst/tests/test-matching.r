
context ("test that every function can take a function name")

truefun <- function (x) TRUE
falsefun <- function (x) FALSE

test_that ("Select can take a function name", {
	expect_equal(mcSelect ("truefun", 1:10), 1:10)
})
test_that ("Reject can take a function name", {
	expect_equal(mcReject ("falsefun", 1:10), 1:10)
})

test_that ("Position can take a function name", {
	expect_equal(mcPosition ("truefun", c('a', 'b', 'c') ), 1)	
})

test_that ("Reduce can take a function name", {
	expect_equal(mcReduce ("+", 1:10), 55)
})

test_that ("Fold can take a function name", {
	expect_equal(mcFold ("+", 0, 1:10), 55)
	
})

test_that ("Find can take a function name", {
	expect_equal(mcFind(truefun, letters[1:10]), 'a')
	
})

test_that ("Partition can take a function name", {
	expect_equal( mcPartition ("truefun", 1:4), list (1:4, integer(0)) )	
})


test_that ("ZipWith can take a function name", {
	
	expect_equal(
		mcZipWith('identity', 1:3, 4:6),
		list (list(1,4), list(2,5), list(3,6)) )

})

test_that ("UnzipWith can take a function name", {
	
	expect_equal(
		mcUnzipWith ('identity', list (list(1,4), list(2,5), list(3,6))),
		list(list(1, 2, 3),	list(4, 5, 6))  )
})

test_that ("All can take a function name", {
	
	expect_equal(mcAll('truefun', 1:10), TRUE)
})

test_that ("Any can take a function name", {
	
	expect_equal(mcOne('truefun', 10:20), FALSE)
})

test_that ("One can take a function name", {
	
	expect_equal(mcAny('truefun', 1:10), TRUE)
	
})
