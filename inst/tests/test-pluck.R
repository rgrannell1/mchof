
context("mcPluck: normal cases")

forall(info = "mcPluck returns correct value when all named, non duplicated",
	list(x_ = r_seq_len(), name_ = r_words(), paropts_ = r_paropts()),
	function (x_, name_, paropts_) {
		
		names(x_) <- rep(name_, length(x_))
		
		identical(
			mcPluck(name_, x_, paropts_),
			unname(x_))
	}	
)

forall(info = "mcPluck returns all key matches in sublists",
	list(x_ = r_seq_len(), name_ = r_words(), paropts_ = r_paropts()),
	function (x_, name_, paropts_) {
		TRUE
	}	
)