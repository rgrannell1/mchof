
context("grouping utils: normal cases")

forall(info = "group_into groups properly",
	list(
		x_ = r_seq_len(),
		size_ = r_integers()),
	function (x_, size_) {

		lengths <- sapply(group_into(x_, size_), length)
		all(head(lengths, -1) %in% c(abs(size_)))
		
	},
	given = function (x_, size_) {
		length(x_) >= abs(size_) - 1
	}
)

forall(info = "group_into values are preserved and in the right order",
	list(x_ = r_seq_len(), size_ = r_integers()),
	function (x_, size_) {
		all(unlist(group_into(x_, size_)) == x_)
	}
)

forall(info = "chop_into returns the correct number of pieces",
	list(x_ = r_seq_len(), pieces_ = r_integers()),
	function (x_, pieces_) {
		length(chop_into(x_, pieces_)) == min(abs(pieces_), length(x_))
	},
	given = function (x_, pieces_) pieces_ != 0
)

