
context("zip & unzip: normal cases")

FLAG("these tests aren't strong enough to verify functions work (mczip)")

forall(info = "zip is an approximate inverse of unzip",
	list(x_ = c(r_int_tuples(), r_empty_list_tuples()), paropts_ = r_paropts()),
	function (x_, paropts_) {
		
		f <- mcUnzip %of% mcZip
		g <- mcZip %of% mcUnzip		
		
		all_equal(list(x_, f(x_, paropts_), g(x_, paropts_)))
	}
)

forall(info = "the length of the outputted tuples is uniform", 
	list(x_ = r_flat_no_null(), paropts_ = r_paropts()),
	function (x_, paropts_) {
		all_equal(sapply(mcZip(x_, paropts_), length)) &&
		all_equal(sapply(mcUnzip(x_, paropts_), length))	
	}
)

forall(info = "structure of the output is preserved for tuples of list( )",
	list(x_ = r_empty_list_tuples(), paropts_ = r_paropts()),
	function (x_, paropts_) {
		
		f <- mcUnzip %of% mcZip
		g <- mcZip %of% mcUnzip
		
		all_equal(list(
			x_,
			f(x_, paropts_),
			g(x_, paropts_) ))
	}
)


