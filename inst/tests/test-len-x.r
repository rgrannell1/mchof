
context("special length cases of x handled consistently")

true_fun <- function(x) TRUE

test_that('NULL behaviour is correct', {
	
	assert('Zipwith, Unzipwith, Reduce, Filter,
			Partition, Find NULL |-> NULL', 
		rule = function (libfn_, fn_, paropts_) {
			is.null(libfn_$f(fn_$f, NULL, paropts_))
		},
		unless = function (libfn_, fn_, x_, paropts_) {
			libfn$name = 'mcPosition' ||
			libfn$name = 'mcZip' || libfn$name = 'mcUnzip'
		},
		where = list(
			libfn = LibFunctions(),	
			fn_ = Functions(),
			paropts_ = Paropts()
		)
	)
	
	assert('mcPosition NULL |-> integer(0)',
		rule = function (fn_, right_, paropts_) {
			res <- mcPosition(fn_, NULL, right_, paropts_)
			is.integer(res) && length(res) == 0
		},
		where = list(
			fn_ = Functions(),
			right_ = Booleans(),
			paropts_ = Paropts()
		)
	)
	
})

test_that('list() behaviour is correct', {
	
	assert('mcFind, mcFilter, mcReduce, mcZipWith,
		mcZip, mcUnzip, mcUnzipWith list() |-> list()',
		rule = function (libfn_, fn_, paropts_) {
			
			if (libfn_$name == 'mcZip' || libfn_$name == 'mcUnzip') {
				res <- libfn_$f(list(), paropts_)
				is.list(res) && length(res) == list()		
			} else {
				res <- libfn_$f(fn_$f, list(), paropts_)
				is.list(res) && length(res) == list()	
			}
		},
		where = list(
			libfn_ = LibFunctions(),
			fn_ = Functions(),
			paropts_ = Paropts()
		)
	)
	
	assert('mcPosition list() |-> integer(0)', 
		rule = function (fn_, right_, paropts_) {
			mcPosition(fn_, list(), right_, paropts_)
		},	   
		where = list (
			fn_ = Functions(),
			right_ = Booleans(),
			paropts_ = Paropts()	
		)
	)
	
	assert('mcPartition list() |-> list( list(), list() )', 
		rule = function (fn_, paropts_) {
			identical(
				mcPartition(fn_, list(), paropts_),
				list( list(), list() )
			)
		},
		where = list (
		   	fn_ = Functions(),
		   	paropts_ = Paropts()
		)
	)
})

## mchof 0.1 functions
test_that("mcPosition length(0) |-> length(0)", {

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
	
	# list() |-> list()
	expect_equal(
		mcZipWith(
			f = identity,	
			list(list())
		), list()
	)
	# list(..., list(), ...) |-> list()
	expect_equal(
		mcZipWith(
			f = identity,	
			list(list(), list(), list(1:10))
		), list()
	)
	
	# list(x1, x2, ...) |-> list( list(x1, x2, ...) )
	expect_equal(
		mcZipWith(
			identity,	
			list(1:3)
		),
		list(list(1), list(2), list(3))
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
	expect_equal(mcZip(NULL), NULL)
	
})

test_that("mcUnzip length(0) |-> length(0)", {
	
	# 0-elements |-> list()
	expect_equal(
		mcUnzip(
			list (
				list ('a', 'b'),
				list (),
				list ('c', 'd')
		)),
		list()	
	)
	
	# list() |-> list()
	expect_equal(
		mcUnzip(list ()),
		list (
			
		))
	
	expect_equal(
		mcUnzip(NULL),
		NULL)
	
	expect_equal(
		mcUnzip(	
			list(list(1:5))	
		),
		list(list(1:5))	)
})

test_that("mcUnzipWith length(0) |-> length(0)", {
	
	# 0-elements |-> 
	expect_equal(
		mcUnzipWith(
			function (x) x,
			list (
				list ('a', 'b'),
				list (),
				list ('c', 'd')
			)),
		list (
			
		))
	
	# list() |-> list()
	expect_equal(
		mcUnzipWith(
		function (x) x,
		list ()),
		list (
			
		))
	
	expect_equal(
		mcUnzipWith(
		function (li) if (is.null(li)) stop ('too damn far!'),		
		NULL),
		NULL)
	
	expect_equal(
		mcUnzipWith(
			function (x) x,
			list(list(1:5))	
		),
		list(list(1:5))	)
	
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