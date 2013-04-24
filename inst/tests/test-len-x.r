context("special length cases of x handled consistently")

true_fun <- function(x) TRUE

test_that('NULL handling behaviour is as expected', {
	
	# NULL |-> integer(0)
	expect_equal(mcPosition(true_fun, integer(0)), integer(0))

	# NULL |-> NULL
	expect_equal(mcFind(true_fun, NULL), NULL)	

	expect_equal(mcFilter(true_fun, NULL), NULL)
	expect_equal(mcPartition(mean, NULL), NULL)

	expect_equal(mcReduce(get('+'), NULL), NULL)

	expect_equal(mcZip(NULL), NULL)
	expect_equal(mcUnzip(NULL), NULL)
	
	expect_equal(mcUnzipWith(mean, NULL), NULL)
	expect_equal(mcZipWith(mean, NULL), NULL)
})

test_that('list() behaviour is defined', {
	
	# list() |-> integer(0)
	expect_equal(mcPosition(true_fun, list()), integer(0))

	expect_equal(mcFilter(true_fun, list()), list())
	# list() |-> list( list(), list() )
	expect_equal(
		mcPartition(identity, list()),	
		list(list(), list())
	)
	
	# list() |-> list()
	expect_equal(mcFind(true_fun, list()), list())	
	expect_equal(mcReduce(get('+'), list()), list())

	expect_equal(mcZip(list()), list())
	expect_equal(mcZip (list (list(), list(1), list(1,2,3))), list())
	expect_equal(mcUnzip(list (list('a', 'b'), list(), list('c', 'd'))), list())
	expect_equal(mcUnzip(list()), list ())
	
	expect_equal(mcUnzipWith(max, list()), list())	
	expect_equal(
		mcUnzipWith(
			function (x) x,
			list (list ('a', 'b'), list (), list ('c', 'd'))), 
		list ())

	expect_equal(mcZipWith(identity, list(list())), list())
	
	# list(..., list(), ...) |-> list()
	expect_equal(mcZipWith(identity, list(list(), list(), list(1:10))), list())		
})

test_that("[A](0) |-> ...", {
	
	# ... integer(0)
	expect_equal(	mcPosition(true_fun, x = character(0)), integer(0))
	
	#...[A](0)
	expect_equal(length(mcFind(function(x) FALSE, character(0))), 0)
	expect_equal(mcFilter(function(x) T, integer(0)), integer(0))

})

test_that("mcReduce length(1) |-> 1", {
	
	expect_equal(mcReduce(get('+'), c(1)), 1)

})

test_that("mcPartition length(0) |-> length(0)", {
	
	expect_equal(
		mcPartition(function(x) TRUE, list(integer(0))), 
		list(list(integer(0)), list()))
	
	expect_equal(
		mcPartition(true_fun, list(1:10)),
		list(list(1:10), list())
	)
})
