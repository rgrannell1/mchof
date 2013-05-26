
context("reduce: normal cases")

forall(
	info = "check that integer addition works",
	list(x_ = r_seq_len(), paropts_ = r_paropts()),
	function (x_, paropts_) {
		mcReduce('+', x_, paropts_) == sum(x_)
	}
)
forall(
	info = "check that concatenation works",
	list(x_ = r_letters(), paropts_ = r_paropts()),
	function (x_, paropts_) {
		mcReduce(paste0, x_, paropts_) == paste0(x_, collapse = '')
	}
)
forall(
	info = "check that small named list concatenation works",
	list(x_ = r_letters(), paropts_ = r_paropts()),
	function (x_, paropts_) {
		concat <- c(x_)
		identical( mcReduce(c, x_, paropts_), concat )
	}
)
