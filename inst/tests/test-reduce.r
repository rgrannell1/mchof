
context("reduce: normal cases")

forall(
	info = "mcReduce + x is equal to sum x",
	list(x_ = r_seq_len(), paropts_ = r_paropts()),
	function (x_, paropts_) {
		mcReduce('+', x_, paropts_) == sum(x_)
	}
)
forall(
	info = "mcReduce paste x is equal to paste x",
	list(x_ = r_letters(), paropts_ = r_paropts()),
	function (x_, paropts_) {
		mcReduce(paste0, x_, paropts_) == paste0(x_, collapse = '')
	}
)