
context("iteratewhile: normal cases")

forall(info = "incrementing 0...n returns n",
	list(n_ = r_integers()),
	function (n_) {

		n_ <- abs(n_)
		mcIterateWhile(
			function (n) n != n_,
			function (n) n + 1, 0) == n_
	}
)

context("mcIndMap: normal cases")

forall(info = "check that indices and values are paired correctly",
	list(
		x_ = c(r_seq_len(), r_seq_len_list()),
		paropts_ = r_paropts()),
	function (x_, paropts_) {

		all(unlist(
			mcIndMap(
				function (i, x) {
					x == i				
				},
				x_,
				paropts_
			)))

	}
)

