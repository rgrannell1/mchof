
context("special length cases of x handled consistently")

true_fun <- function(x) TRUE

## mchof 0.1 functions

test_that("mcPosition length(0) |-> length(0)", {
	
	# [A](0) |-> integer(0)
	expect_equal(	
		mcPosition(
			true_fun, x = character(0)),
		integer(0))
	
	# NULL |-> integer(0)
	expect_equal(	
		mcPosition(
			true_fun, x = integer(0)),
		integer(0))
	
	# list() |-> integer(0)
	expect_equal(	
		mcPosition(
			true_fun, list()),
		integer(0))
})

test_that("mcFind length(0) |-> length(0)", {

	expect_equal(
		length(mcFind(
			function(x) FALSE, 
			character(0)
		)),	0)
	
	# mcFind NULL |-> NULL
	expect_equal(
		mcFind(true_fun, NULL),	
		NULL)
	
	# list() |-> list()
	expect_equal(
		mcFind(true_fun, list()),	
		list())
})

test_that("mcFilter length(0) |-> length(0)", {
	
	# NULL |-> NULL
	expect_equal(
		mcFilter(
			function(x) T,
			NULL
		), NULL)
	
	# list() |-> list()
	expect_equal(
		mcFilter(
			function(x) T,
			list()
		), list())
	
	# [A](0) |-> [A](0)
	expect_equal(
		mcFilter(
			function(x) T,
			integer(0)
		), integer(0))
})

test_that("mcReduce length(0) |-> length(0) & length(1) |-> 1", {

	# len(x) == 1 |-> 1
	expect_equal(
		mcReduce(get('+'), c(1)), 
		1)
	
	# NULL |-> NULL 
	expect_equal(
		mcReduce(get('+'), NULL),
		NULL)
	# list() |-> list()
	expect_equal(
		mcReduce(get('+'), list()),
		list())
})

## mchof 0.2 functions
#=#=#=#=#=#=#=#=#=#=#=#
#=#=#=#=#=#=#=#=#=#=#=#

test_that("mcZipWith length(0) |-> length(0)", {
	
	# NULL |-> NULL 
	expect_equal(
		mcZipWith(
			f = identity,
			NULL),	
		NULL
	)
	
	# list() |-> list(list())
	expect_equal(
		mcZipWith(
			f = identity,	
			list(list())
		), list(list())
	)
	# list() |-> list() |-> list(list(), list())
	expect_equal(
		mcZipWith(
			f = identity,	
			list(list(), list(), list(1:10))
		), list(list(list(), list(), list()))
	)
	
	# list(x1, x2, ...) |-> list( list(x1, x2, ...) )
	expect_equal(
		mcZipWith(
			identity,	
			list(1:10)
		),
		list(list(1:10))
	)
	
	
})

test_that("mcZip length(0) |-> length(0)", {

	# 0-elements |-> list()
	expect_equal(
		mcZip (
			list (
				list(),		
				list(1),
				list(1,2,3)
			),
			paropts = list(mc.cores = 5)	
		),
		list())
	
	# list() |-> list()
	expect_equal(
		mcZip(
			list (),
			paropts = list(mc.cores = 1)),
		list())
	
	# one-list |-> one-list
	expect_equal(
		mcZip(
			list(list(1:5))	
		),
		list(list(1:5)))
	
	# NULL |-> NULL
	expect_equal(
		mcZip(
			NULL
		),	
		NULL)	
})

test_that("mcUnzip length(0) |-> length(0)", {
		
})

test_that("mcPartition length(0) |-> length(0)", {
	
	
	# list() |-> list( list(), list() )
	
	expect_equal(
		mcPartition(
			function (x) identity,
			list()
		),	
		list(list(), list())
	)
	expect_equal(
		mcPartition(
			function(x) TRUE,
			NULL
		), NULL)
	
	expect_equal(
		mcPartition(
			function(x) T,
			list(integer(0))
		), 
		list(list(integer(0)), list()))
	
	expect_equal(
		mcPartition(
			function(x) T,	
			list(1:10)),
		list(list(1:10), list())
	)
})

