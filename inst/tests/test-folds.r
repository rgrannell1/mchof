
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
forall(
	info = "mcReduce head x is equal to x",
	list(x_ = r_letters(), paropts_ = r_paropts()),
	function (x_, paropts_) {
		all_equal(list(
			mcReduce(function (x) stop(""), head(x_, 1), paropts_),		
			head(x_, 1)
		))
	}
)

context("fold: normal cases")
	
forall(
	info = "fold + 0 x is equal to sum x",
	list(x_ = r_seq_len(), paropts_ = r_paropts()),
	function (x_, paropts_) {
		mcFold('+', 0, x_, paropts_) == sum(x_)
	}
)
forall(
	info = "mcfold paste x is equal to paste x",
	list(x_ = r_letters(), paropts_ = r_paropts()),
	function (x_, paropts_) {
		mcFold(paste0, '', x_, paropts_) == paste0(x_, collapse = '')
	}
)
forall(
	info = "check that list accumulation works properly",
	list(x_ = r_small_named_list(), paropts_ = r_paropts()),
	function (x_, paropts_) {
		identical(mcFold(
			function (acc, new) c(acc, unname(new)),
			NULL, x_, paropts_), unname(unlist(x_)))
	}
)