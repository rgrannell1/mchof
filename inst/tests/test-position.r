
forall(info = "position always returns a single integer, or none",
	list(func_ = list(
		function (...) TRUE, function (...) FALSE),
		x_ = r_flat_no_null(), paropts_ = r_paropts()),
	function (func_, x_, paropts_) {
		
		res <- mcPosition(func_, x_, TRUE, paropts_)
		is.integer(res) && length(res) == 0 || length(res) == 1
		
	}
)

FLAG("add more tests")
