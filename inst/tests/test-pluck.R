
context("mcPluck: normal cases")

forall(
	info = "mcPluck returns correct value when all named, duplicated vector",
	list(x_ = r_seq_len(), name_ = r_words(), paropts_ = r_paropts()),
	function (x_, name_, paropts_) {
		
		names(x_) <- sample(c(name_, rev(name_)), size = 1, replace = TRUE)
		
		identical(
			mcPluck(name_, x_, paropts_),
			unname(x_[ which(names(x_) == name_) ]))
	}	
)

forall(info = "mcPluck returns all key matches in fully named sublists",
	list(x_ = r_seq_len(), name_ = r_words(), paropts_ = r_paropts()),
	function (x_, name_, paropts_) {
		TRUE
	}	
)

forall(info = "mcPluck returns all key matches in partially named sublists",
	list(x_ = r_seq_len(), name_ = r_words(), paropts_ = r_paropts()),
	function (x_, name_, paropts_) {
		TRUE
	}	
)
