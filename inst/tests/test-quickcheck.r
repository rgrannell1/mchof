
context('quickcheck functions where possible')

denullify <- function (x) {
	# remove the null values from x
	
	null_ind <- which( sapply(x, is.null) )
	if (length(null_ind) > 0) x[-null_ind] else x
}

forall(
	list(x_ = r_flat_no_null(1000), paropts_ = r_paropts(4)),
	info = 'mcFilter, !mcReject & mcPartition & which are equivelent',
	function (x_, paropts_) {
		
		res_0 <- denullify(x_)
		res_1 <- mcFilter(Negate(is.null), x_, paropts_)
		res_2 <- mcReject(is.null, x_, paropts_)
		res_3 <- mcPartition(is.null, x_, paropts_)[[2]]

		length_unique <- length(unique(list(res_0, res_1, res_2, res_3)))

		length_unique == 1
	}, opts = list(time = 2)
)

forall(
	list(x_ = r_integers(1000), paropts_ = r_paropts(4)),
	info = 'reduce & fold addition over integers works',
	function (x_, paropts_) {
		
		res_0 <- sum(x_)
		res_1 <- mcFold('+', 0, x_, paropts_)
		res_2 <- mcReduce('+', x_, paropts_)

		length(unique(res_0, res_1, res_2)) == 1
	
	}, opts = list(time = 2)
)

forall(
	list(x_ = r_integers(1000), paropts_ = r_paropts(4)),
	info = 'reduce & fold multiplication over integers works',
	function (x_, paropts_) {
		
		res_0 <- prod(x_)
		res_1 <- mcFold('*', 1, x_, paropts_)
		res_2 <- mcReduce('*', x_, paropts_)
		
		length(unique(res_0, res_1, res_2)) == 1
		
	}, opts = list(time = 2)
)












