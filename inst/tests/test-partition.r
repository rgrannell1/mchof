
context ("test that mcPartition is well behaved for normal cases")

forall(info = "true cases first, then false cases",
	list(x_ = r_seq_len(), paropts_ = r_paropts()),
	function (x_, paropts_) {
		res <- mcPartition(function (x) TRUE, x_)
		identical( res[[1]], x_ )
	}
)
forall(info = "if x is a list, an empty slot is list()",
	list(x_ = r_flatlist(), paropts_ = r_paropts()),
	function (x_, paropts_) {
		res <- mcPartition(function (x) TRUE, x_)
		identical( res[[2]], list() )
	},
	given = function (x_, paropts_) is.list(x_)
)
forall(info = "if x is an int vector, an empty slot is integer()",
	list(x_ = r_seq_len(), paropts_ = r_paropts()),
	function (x_, paropts_) {
		res <- mcPartition(function (x) TRUE, x_)
		identical(res[[2]], integer(0))
	}
)
forall(info = "if x is a char vector, an empty slot is integer()",
	list(x_ = r_letters(), paropts_ = r_paropts()),
	function (x_, paropts_) {
		res <- mcPartition(function (x) TRUE, x_)
		identical(res[[2]], character(0))
	}
)
