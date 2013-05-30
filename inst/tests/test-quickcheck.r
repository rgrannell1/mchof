
context('quickcheck functions where possible')

FLAG("need to remove this file")

denullify <- function (x) {
	# remove the null values from x
	
	null_ind <- which( sapply(x, is.null) )
	if (length(null_ind) > 0) x[-null_ind] else x
}
all_equal <- function (x) {
	length(unique(x)) == 1	
}

forall(
	list(x_ = r_flat_no_null(1000), paropts_ = r_paropts(4)),
	info = 'mcFilter, !mcReject, mcPartition & a nullless list which are equivelent',
	function (x_, paropts_) {
		
		res_0 <- denullify(x_)
		res_1 <- mcFilter(Negate(is.null), x_, paropts_)
		res_2 <- mcReject(is.null, x_, paropts_)
		res_3 <- mcPartition(is.null, x_, paropts_)[[2]]

		all_equal(list(res_0, res_1, res_2, res_3))

	}, opts = list(time = 2)
)

forall(
	list(x_ = r_integers(1000), paropts_ = r_paropts(4)),
	info = 'reduce * [1...n] is fold * [1...n] is [1 * 2 * ... * n]',
	function (x_, paropts_) {
		
		res_0 <- prod(x_)
		res_1 <- mcFold('*', 1, x_, paropts_)
		res_2 <- mcReduce('*', x_, paropts_)
		
		length(unique(res_0, res_1, res_2)) == 1
		
	}, opts = list(time = 2)
)

forall(
	list(x_ = r_tuple_list(1000), paropts = r_paropts(4)),
	info = 'zip then unzip ~ original',
	function (x_, paropts_) {

		identity_2 <- mcUnzip %of% mcZip
		identity_3 <- mcZip %of% mcUnzip		

		all_equal( list(x_, identity_2(x_, paropts_), identity_3(x_, paropts_)) )
	},
	given = function (x_, paropts_) {
		all_equal(sapply(x_, length))
	}
)
