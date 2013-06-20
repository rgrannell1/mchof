
context("mcReduce, mcReducel, mcReducer: normal cases")

forall(
	info = "mcReduce + x is equal to sum x",
	list(x_ = r_seq_len(), paropts_ = r_paropts()),
	function (x_, paropts_) {

		res_1 <- mcReduce('+', x_, paropts_) == sum(x_)
		res_2 <- mcReducel('+', x_) == sum(x_)
		res_3 <- mcReducer('+', x_) == sum(x_)

		res_1 && res_2 && res_3
	}
)
forall(
	info = "mcReduce paste x is equal to paste x",
	list(x_ = r_letters(), paropts_ = r_paropts()),
	function (x_, paropts_) {

		res_1 <- mcReduce(paste0, x_, paropts_) == paste0(x_, collapse = '')
		res_2 <- mcReducel(paste0, x_) == paste0(x_, collapse = '')
		res_3 <- mcReducer(paste0, x_) == paste0(x_, collapse = '')

		res_1 && res_2 && res_3
	}
)

context("mcFold, mcFoldr, mcFoldl: normal cases")
	
forall(
	info = "fold + 0 x is equal to sum x",
	list(x_ = r_seq_len(), paropts_ = r_paropts()),
	function (x_, paropts_) {

		res_1 <- mcFold('+', 0, x_, paropts_) == sum(x_)
		res_2 <- mcFoldl('+', 0, x_) == sum(x_)
		res_3 <- mcFoldr('+', 0, x_) == sum(x_)

		res_1 && res_2 && res_3
	}
)
forall(
	info = "fold paste x is equal to paste x",
	list(x_ = r_letters(), paropts_ = r_paropts()),
	function (x_, paropts_) {

		res_1 <- mcFold(paste0, '', x_, paropts_) == paste0(x_, collapse = '')
		res_2 <- mcFoldl(paste0, '', x_) == paste0(x_, collapse = '')
		res_3 <- mcFoldr(paste0, '', x_) == paste0(x_, collapse = '')
		
		res_1 && res_2 && res_3
	}
)
forall(
	info = "fold list accumulation works properly",
	list(x_ = r_small_named_list(), paropts_ = r_paropts()),
	function (x_, paropts_) {
		
		acc_fun <- function (acc, new) c(acc, unname(new))
		res_1 <- mcFold(acc_fun, c(), x_, paropts)
		res_2 <- mcFoldl(acc_fun, c(), x_)
		res_3 <- mcFoldr(acc_fun, c(), x_)

		setequal(res_1, res_2) &&
		setequal(res_1, res_3)
	}
)

forall(
	info = "stacking values into a list works with foldl & foldr",
	list(x_ = r_seq_len()),
	function (x_) {

		all_equal(list(
			mcFoldl(
				function (acc, new) {
					c(acc, list(new))
				},
				list(), x_),
			mcFoldr(
				function (new, acc) {
					c(list(new), acc)
				},
				list(), x_),
			as.list(x_),
			Reduce(
				function (acc, new) {
					c(acc, list(new))
				},
				init = list(), x = x_)))

	}
)
















