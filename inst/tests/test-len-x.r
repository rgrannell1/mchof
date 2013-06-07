
true_fun <- function (x) TRUE

context("nulls handled correctly")
#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#
#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#

forall(info = "mcPosition f x = NULL paropts |-> integer(0)",
	list(
		f_ = list(mean, max, mode), 
		right_ = list(TRUE, FALSE), paropts_ = r_paropts()),
	function (f_, right_, paropts_) {

		res <- mcPosition(
			f = f_, x = NULL, right = right_, paropts = paropts_)
		ISSUE("is this right? position")
		is.integer(res) && length(res) == 0
	}
)

forall(info = "non-variadic functions that take x = NULL |-> NULL",
	list(
		func_ = list(
			mcAll, mcAny, mcFilter, mcFind, mcOne, mcPartition,
			mcReject, mcSelect, mcUnzip, mcUnzipWith
		),
		f_ = list(mean, max, mode), 
		right_ = list(TRUE, FALSE), 
		paropts_ = r_paropts()	
	),
	function (func_, f_, right_, paropts_) {
		is.null(adapt_call(
			func_,
			with = list(f = f_, right = right_, x = NULL, paropts = paropts_)))
	}
)

forall(
	info = "mcZip & mcZipWith NULL |-> NULL",
	list(paropts_ = r_paropts()),
	function (paropts_) {
		is.null(mcZip(NULL, paropts = paropts_)) && 
		is.null(mcZipWith(mean, NULL, paropts = paropts_))
	}	
)

context("check that A[0] behaviour is defined")
#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#
#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#

forall(
	info = "mcAll [A](0) |-> TRUE",
	list(f_ = list(mean, max, mode), x_ = r_vector_0(), paropts = r_paropts()),
	function (f_, x_, paropts_) {
		mcAll(f_, x_, paropts_)
	}
)
forall(
	info = "mcAny & mcOne [A](0) |-> FALSE",
	list(f_ = list(mean, max, mode), x_ = r_vector_0(), paropts = r_paropts()),
	function (f_, x_, paropts_) {
		!mcAny(f_, x_, paropts_) && 
		!mcOne(f_, x_, paropts_)
	}
)
forall(
	info = "Zip, ZipWith [A](0) |-> list()",
	list(f_ = list(mean, max, mode), x_ = r_vector_0(), paropts = r_paropts()),
	function (f_, x_, paropts_) {
		is_list0(mcZip(x_, paropts = paropts_)) && 
		is_list0(mcZip(x_, x_, paropts = paropts_)) &&
		is_list0(mcZipWith(f_, x_, x_, paropts = paropts_))
	}
)
forall(
	info = "Unzip, UnzipWith [A](0) |-> list()",
	list(f_ = list(mean, max, mode), x_ = r_vector_0(), paropts = r_paropts()),
	function (f_, x_, paropts_) {
		is_list0(mcUnzip(x_, paropts = paropts_)) &&
		is_list0(mcUnzip(list(x_, x_), paropts = paropts_)) 
		is_list0(mcUnzipWith(f_, x_, paropts = paropts_))
	}
)

forall(
	info = "Filter, Reject [A](0) |-> [A](0)",
	list(
		func_ = list(mcFilter, mcReject, mcSelect),
		f_ = list(mean, max, mode),
		x_ = r_vector_0(),
		paropts = r_paropts()),
	function (func_, f_, x_, paropts_) {
		identical(func_(f_, x_, paropts_), x_)
	}
)

forall(info = "mcPosition f x [A](0) |-> integer(0)",
	list(
		f_ = list(mean, max, mode), x_ = r_vector_0(), 
		right_ = list(TRUE, FALSE), paropts_ = r_paropts()),
	function (f_, x_, right_, paropts_) {
		res <- mcPosition(
			f_, x_, right = right_, paropts = paropts_)
		is.integer(res) && length(res) == 0
	}
)

forall(info = "mcFind f [A](0) |-> [A](0)",
	list(
		f_ = list(mean, max, mode), x_ = r_vector_0(), paropts_ = r_paropts()),
	function (f_, x_, paropts_) {
		identical(mcFind(f_, x_, paropts_), x_)
	}
)

ISSUE("define [A](0) for reduce and fold")

context("check that empty lists are handled correctly")
#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#
#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#

forall(
	info = "mcAll list() |-> TRUE",
	list(f_ = list(mean, max, mode), paropts = r_paropts()),
	function (f_, paropts_) {
		ISSUE("define all list() behaviour")
		mcAll(f_, list(), paropts_)
	}
)

forall(
	info = "mcAny & mcOne list() |-> FALSE",
	list(f_ = list(mean, max, mode), paropts = r_paropts()),
	function (f_, paropts_) {
		ISSUE("define any list() behaviour")
		!mcAny(f_, list(), paropts_) && 
		!mcOne(f_, list(), paropts_)
	}
)

test_that('list() behaviour is defined', {
	
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
	info = "mcReduce ",
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
