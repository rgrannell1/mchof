
forall(info = "in the sequence 1...n, the nth element is n",
	list(n_ = r_integers(), x_ = r_seq_len(), paropts_ = r_paropts()),
	function (n_, x_, paropts_) {
		
		val_one <- mcPosition(
			function (y) y == n_, x_, FALSE, paropts_)
		val_two <- mcPosition(
			function (y) y == n_, rev(x_), right = TRUE, paropts_)
		val_one == n && val_two == n_
	
	},
	given = function (n_, x_, paropts_) n_ %in% x_
)

FLAG("add more tests")