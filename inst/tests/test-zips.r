
context("zip & unzip: normal cases")

forall(info = 'zip <-> unzip ~ original',
	list(x_ = c(r_tuple_list(), r_empty_lists()), paropts_ = r_paropts()),
	function (x_, paropts_) {
		
		f <- mcUnzip %of% mcZip
		g <- mcZip %of% mcUnzip		
		
		all_equal(list(x_, f(x_, paropts_), g(x_, paropts_)))
	},
	given = function (x_, paropts_) {
		all_equal(sapply(x_, length))
	}
)

forall(info = "output length is uniform", 
	list(x_ = r_flat_no_null(), paropts_ = r_paropts()),
	function (x_, paropts_) {
		all_equal(sapply(mcZip(x_, paropts_), length)) &&
		all_equal(sapply(mcUnzip(x_, paropts_), length))	
	}
)

context("zipwith & unzipwith: normal cases")

forall(info = "check that functions are being applied to tuples",
	list(x_ = r_tuple_list(), paropts_ = r_paropts()),
	function (x_, paropts_) {
		mcUnzipWith(
			function (n) list(unlist(n)^2), x_, paropts_)
	}
)
