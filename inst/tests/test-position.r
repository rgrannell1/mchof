
forall(info = "in the sequence 1...n, the nth element is n",
	list(n_ = 1:20, x_ = r_seq_len(), paropts_ = r_paropts()),
	function (n_, x_, paropts_) {

		val_one <- mcPosition(
			function (y) y == n_, x_, FALSE, paropts_)
		val_two <- mcPosition(
			function (y) y == n_, rev(x_), right = TRUE, paropts_)
		val_one == n_ && val_two == n_
	
	},
	given = function (n_, x_, paropts_) n_ %in% x_
)

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
