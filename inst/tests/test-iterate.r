
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
