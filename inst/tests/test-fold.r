
context("fold: normal cases")
	
forall(
	info = "check that integer addition works",
	list(x_ = r_seq_len(), paropts_ = r_paropts()),
	function (x_, paropts_) {
		mcFold('+', 0, x_, paropts_) == sum(x_)
	}
)
forall(
	info = "check that concatenation works",
	list(x_ = r_letters(), paropts_ = r_paropts()),
	function (x_, paropts_) {
		mcFold(paste0, '', x_, paropts_) == paste0(x_, collapse = '')
	}
)
forall(
	info = "check that small named list concatenation works",
	list(x_ = r_letters(), paropts_ = r_paropts()),
	function (x_, paropts_) {
	
	}
)

FLAG()
