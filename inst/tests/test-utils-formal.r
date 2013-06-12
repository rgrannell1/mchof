
context("formal utilities: normal cases")

forall(
	info = "equal functions are equal",
	list(
		f_ = list(
			mcReduce, mcFold, mcFind, mcFilter, mcReject,
			mcMultiply, mcNot, mcZipWith)),
	function (f_) {
		formal_names_equal(f_, f_) &&
		formal_defaults_equal(f_, f_) &&
		formals_equal(f_, f_)
	}
)

forall(
	info = "non-equal functions are non-equal",
	list(
		f_ = list(
			mcReduce, mcFold, mcFind, mcFilter, mcReject,
			mcMultiply, mcNot, mcZipWith),
		g_ = list(
			mean, max, mode, Position	
		)),
	function (f_, g_) {
		!formals_equal(f_, g_)
	}
)