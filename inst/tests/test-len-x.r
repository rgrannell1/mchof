context("special length cases of x handled consistently")

true_fun <- function (x) TRUE

FLAG("special cases need to be nailed down for 0.3")

x_as_null <- list(
	mcAll = curry(mcAll, x = NULL),
	mcAny = curry(mcAny, x = NULL),
	mcFilter = curry(mcFilter, x = NULL),
	mcFind = curry(mcFind, x = NULL),
	mcFold = curry(mcFold, x = NULL),
	mcOne = curry(mcOne, x = NULL),
	mcPartition = curry(mcPartition, x = NULL),
	mcPosition = curry(mcPosition, x = NULL),
	mcReduce = curry(mcReduce, x = NULL),
	mcReject = curry(mcReject, x = NULL),
	mcSelect = curry(mcSelect, x = NULL),
	mcUnzip = curry(mcUnzip, x = NULL),
	mcUnzipWith = curry(mcUnzipWith, x = NULL),
	mcZip = curry(mcZip, x = NULL),
	mcZipWith = curry(mcZipWith, x = NULL)
)

forall(info = "mcPosition x = NULL returns integer(0)",
	list(
		f_ = list(mean, max, mode), 
		right_ = list(TRUE, FALSE), paropts_ = r_paropts()),
	function (f_, right_, paropts_) {
		res <- x_as_null$mcPosition(f = f_, right = right_, paropts = paropts_) 
		is.integer(res) && length(res) == 0
	}
)

forall(info = "quantifiers, filters, find x = NULL returns NULL",

)

test_that('NULL handling behaviour is as expected', {

	# NULL |-> NULL
	expect_equal(mcFind(true_fun, NULL), NULL)	

	expect_equal(mcSelect(true_fun, NULL), NULL)
	expect_equal(mcReject(true_fun, NULL), NULL)
	expect_equal(mcPartition(mean, NULL), NULL)

	expect_equal(mcReduce('+', NULL), NULL)
	expect_equal(mcFold('+', 0, NULL), NULL)

	expect_equal(mcZip(NULL), NULL)
	expect_equal(mcUnzip(NULL), NULL)
	
	expect_equal(mcUnzipWith(mean, NULL), NULL)
	expect_equal(mcZipWith(mean, NULL), NULL)
	
	expect_equal(mcAll(mean, NULL), NULL)
	expect_equal(mcAny(mean, NULL), NULL)
	expect_equal(mcOne(mean, NULL), NULL)
})

test_that('list() behaviour is defined', {
	
	# list() |-> integer(0)
	expect_equal(mcPosition(true_fun, list()), integer(0))

	# list() |-> list( list(), list() )
	expect_equal(
		mcPartition(identity, list()),	
		list(list(), list())
	)
	
	# list() |-> list()
	expect_equal(mcFind(true_fun, list()), list())	
	expect_equal(mcReduce('+', list()), list())
	
	expect_equal(mcZip(), list())
	expect_equal(mcZip (list(), list(1), list(1,2,3)), list())
	expect_equal(mcUnzip(list(list('a', 'b'), list(), list('c', 'd'))), list())
	expect_equal(mcUnzip(list()), list ())
	
	expect_equal(mcSelect(true_fun, list()), list())
	expect_equal(mcReject(true_fun, list()), list())
	
	expect_equal(mcUnzipWith(max, list()), list())	
	expect_equal(
		mcUnzipWith(identity,
			list( list ('a', 'b'), list (), list ('c', 'd')) ), 
		list ())

	expect_equal(mcZipWith(identity, list()), list())
	
	# list(..., list(), ...) |-> list()
	expect_equal(mcZipWith(identity, list(), list(), list(1:10)), list())
	
	# quantify list() -> list()
	expect_equal(mcAll(mean, list()), list())
	expect_equal(mcAny(mean, list()), list())
	expect_equal(mcOne(mean, list()), list())
	
})

test_that("[A](0) |-> ...", {
	
	# ... integer(0)
	expect_equal(mcPosition(true_fun, x = character(0)), integer(0))
	
	#...[A](0)
	expect_equal(length(mcFind(function(x) FALSE, character(0))), 0)
	expect_equal(mcFilter(function(x) T, integer(0)), integer(0))

})

forall(
	info = "mcReduce x[1] |-> x[1]",
	list(x_ = c(r_letters(), r_integers(), r_flat_no_null()), paropts_ = r_paropts()),
	function (x_, paropts_) {
		all_equal(list(
			mcReduce(function (x) stop("too late!"), head(x_, 1), paropts_),		
			head(x_, 1)
		))
	}
)

test_that("mcPartition length(0) |-> length(0)", {
	
	expect_equal(
		mcPartition(function(x) TRUE, list(integer(0))), 
		list(list(integer(0)), list()))
	
	expect_equal(
		mcPartition(true_fun, list(1:10)),
		list(list(1:10), list())
	)
})
