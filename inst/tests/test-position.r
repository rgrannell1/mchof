
forall(info = "position always returns a single integer, or none",
	list(func_ = list(
		function (...) TRUE, function (...) FALSE),
		x_ = r_flat_no_null(), paropts_ = r_paropts()),
	function (func_, x_, paropts_) {
		
		res <- mcPosition(func_, x_, TRUE, paropts_)
		is.integer(res) && length(res) == 0 || length(res) == 1
		
	}
)

forall(info = "position of the ith letter is i",
	list(ind_ = 1:26, paropts_ = r_paropts()),
	function (ind_, paropts_) {
		
		mcPosition(function (letter) letter == letters[ind_], letters) == ind_
	}
)

forall(info = "position always returns the first match",
	list(x_ = r_int_vectors(), paropts_ = r_paropts()),
	function (x_, paropts_) {
		
		elem <- x_[sample(seq_along(x_), size = 1)]
		mcPosition(function (x) x == elem, x_, FALSE, paropts_) == which(x_ == elem)[1]
	},
	given = function (x_, paropts_) length(x_) > 0
)

forall(info = "position right = T always returns the first match",
	list(x_ = r_int_vectors(), paropts_ = r_paropts()),
	function (x_, paropts_) {
		
		elem <- x_[sample(seq_along(x_), size = 1)]
		res <- mcPosition(function (x) x == elem, x_, TRUE, paropts_)

		res == max(which(x_ == elem))
	},
	given = function (x_, paropts_) length(x_) > 0
)
