
context ("test that mcPartition is well behaved for normal cases")

forall(info = "true cases first, then false cases",
	list(x_ = r_seq_len(100), paropts_ = r_paropts()),
	function (x_, paropts_) {
		res <- mcPartition(function (x) TRUE, x_)
		identical( res[[1]], x_ )
	}
)
forall(info = "if x is a list, an empty slot is list()",
	list(x_ = r_flatlist(100), paropts_ = r_paropts()),
	function (x_, paropts_) {
		res <- mcPartition(function (x) TRUE, x_)
		identical( res[[2]], list() )
	},
	given = function (x_, paropts_) is.list(x_)
)
