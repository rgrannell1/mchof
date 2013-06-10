
true_fun <- function (x) TRUE

context("nulls handled correctly")

forall(info = "non-variadic functions that take x = NULL |-> NULL",
	list(
		func_ = list(
			mcAll, mcAny, mcFilter, mcFind, mcFold, mcOne, mcPartition, mcPosition,
			mcReject, mcSelect, mcUnzip, mcUnzipWith
		),
		f_ = list(mean, max, mode), 
		right_ = list(TRUE, FALSE), first_ = r_integers(),
		paropts_ = r_paropts()	
	),
	function (func_, f_, first_, right_, paropts_) {
		is.null(adapt_call(
			func_,
			with = list(
				f = f_, first = first_,
				right = right_, x = NULL, paropts = paropts_)))
	}
)

forall(
	info = "mcZip & mcZipWith NULL |-> NULL",
	list(paropts_ = r_paropts()),
	function (paropts_) {
		is.null(mcZip(NULL, paropts = paropts_)) && 
		is.null(mcZipWith(squash(mean), NULL, paropts = paropts_))
	}	
)

context("check that A[0] behaviour is defined")

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
		is_list0(mcZipWith(squash(f_), x_, x_, paropts = paropts_))
	}
)
forall(info = "Unzip, UnzipWith [A](0) |-> list()",
	list(f_ = list(mean, max, mode), x_ = r_vector_0(), paropts = r_paropts()),
	function (f_, x_, paropts_) {
		is_list0(mcUnzip(x_, paropts = paropts_)) &&
		is_list0(mcUnzip(list(x_, x_), paropts = paropts_)) &&
		is_list0(mcUnzipWith(squash(f_), x_, paropts = paropts_))
	}
)

forall(info = "Filter, Reject [A](0) |-> [A](0)",
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
		is_integer0(res)
	}
)

forall(info = "mcFind f [A](0) |-> [A](0)",
	list(
		f_ = list(mean, max, mode), x_ = r_vector_0(), paropts_ = r_paropts()),
	function (f_, x_, paropts_) {
		identical(mcFind(f_, x_, paropts_), x_)
	}
)

forall(info = "mcPartition true [A](0) |-> list(..., [A](0))",
	list(x_ = r_typed_vectors(), paropts_ = r_paropts()),
	function (x_, paropts_) {
		identical( mcPartition(true_fun, x_, paropts_)[[2]], x_[0])
	}
)

context("check that empty lists are handled correctly")

forall(
	info = "mcAll list() |-> TRUE", 
	list(f_ = list(mean, max, mode), paropts = r_paropts()),
	function (f_, paropts_) {
		mcAll(f_, list(), paropts_)
	}
)

forall(
	info = "mcAny & mcOne list() |-> FALSE",
	list(f_ = list(mean, max, mode), paropts = r_paropts()),
	function (f_, paropts_) {
		!mcAny(f_, list(), paropts_) && 
		!mcOne(f_, list(), paropts_)
	}
)

forall(
	info = "mcSelect, mcReject, mcFind, mcReduce list() |-> list()",
	list(
		func_ = list(mcSelect, mcReject, mcFind, mcReduce),
		f_ = list(mean, max, mode), right_ = list(TRUE, FALSE),
		paropts = r_paropts()),
	function (func_, f_, right_, paropts_) {
		is_list0( adapt_call(func_, with = list(
			f = f_, x = list(), right = right_, paropts = paropts_ 	
		)) )
	}
)

forall(
	info = "mcPartition list() |-> list( list(), list() )",
	list(f_ = list(mean, max, mode), paropts = r_paropts()),
	function (f_, paropts_) {
		identical(
			mcPartition(f_, list(), paropts_),
			list( list(), list() ))
	}
)

forall(
	info = "mcZip list(), ... |-> list()",
	list(x_ = r_tuple_list(), paropts_ = r_paropts()), 
	function (x_, paropts_) {
		is_list0( do.call(mcZip, list(x_, list(), paropts = paropts_)) )
	}
)

context("special tests for mcReduce and mcFold")

forall(
	info = "mcReduce length(one) -> length(one)",
	list(x_ = c(r_letters(), r_integers(), r_flat_no_null()), paropts_ = r_paropts()),
	function (x_, paropts_) {
		all_equal(list(
			mcReduce(function (x) stop("too late!"), head(x_, 1), paropts_),		
			head(x_, 1)
		))
	}
)

forall(
	info = "mcFold first length(0) |-> first",
	list(
		f_ = list(mean, max, mode), first_ = r_integers(),
		x_ = c(list(), r_vector_0()), paropts_ = r_paropts()),
	function (f_, first_, x_, paropts_) {
		identical(mcFold(f_, first_, x_, paropts_), first_)
	}
)
