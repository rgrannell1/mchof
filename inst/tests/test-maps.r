
context("mcIndMap: normal cases")

forall(info = "check that indices and values are paired correctly",
	list(
		x_ = r_seq_len(),
		paropts_ = r_paropts()),
	function (x_, paropts_) {

		all(unlist(
			mcIndMap(
				function (i, x) {
					x == i				
				},
				x_,
				paropts_
			))) && all(unlist(
			mcIndMap(
				function (i, x) {
					x == i				
				},
				as.list(x_),
				paropts_
			)))
	}
)

