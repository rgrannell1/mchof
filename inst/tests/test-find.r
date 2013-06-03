
context("Find: normal cases")

forall(info = "in the sequence 1...n, the nth element is n",
	list(n_ = 1:20, x_ = r_seq_len(), paropts_ = r_paropts()),
	function (n_, x_, paropts_) {
		val_one <- mcFind(function (y) y == n_, x_, FALSE, paropts_)
		val_two <- mcFind(function (y) y == n_, rev(x_), right = TRUE, paropts_)
		val_one == n_ && val_two == n_
	},
	given = function (n_, x_, paropts_) n_ %in% x_
)

forall(info = "The ith letter is letter[i]",
	list(ind_ = 1:26, paropts_ = r_paropts()),
	function (ind_, paropts_) {
		mcFind(function (letter) letter == letters[ind_], letters) == letters[ind_]
	}
)