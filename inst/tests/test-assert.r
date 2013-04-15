
context("check that assert works")

assert ('a + b == b + a',

	rule = function (a_, b_) a_ + b_ == b_ + a_,
	where = list(a = seq_len(100), b = seq_len(100))
)

assert ('the sum of terms is larger than the terms, 
	given some are larger than zero', 

	rule = function (a_, b_, c_) {
		a_ + b_ + c_ > max(a_, b_, c_)
	},
	given = function (a_, b_, c_) {
		length(which(c(a_, b_, c_) == 0)) < 2
	}
	where = list(
		a_ = 0:100,
		b_ = 0:100,
		c_ = 0:100)
)

