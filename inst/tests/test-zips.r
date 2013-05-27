
context("zip: normal cases")

forall(
	list(x_ = r_tuple_list(), paropts_ = r_paropts()),
	info = 'zip <-> unzip ~ original',
	function (x_, paropts_) {
		
		identity_2 <- mcUnzip %of% mcZip
		identity_3 <- mcZip %of% mcUnzip		
		
		all_equal(list(x_, identity_2(x_, paropts_), identity_3(x_, paropts_)))
	},
	given = function (x_, paropts_) {
		all_equal(sapply(x_, length))
	}
)

