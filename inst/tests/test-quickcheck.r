
context('quickcheck functions where possible')

denullify <- function (x) {
	null_ind <- which( sapply(x, is.null) )

	if (length(null_ind) > 0) {
		x[-null_ind] 
	} else x
}

forall(
	list(x_ = r_flat_no_null(1000), paropts_ = r_paropts(2)),
	function (x_, paropts_) {
		
		res_0 <- denullify(x_)
		res_1 <- mcFilter(Negate(is.null), x_, paropts_)
		res_2 <- mcReject(is.null, x_, paropts_)
		res_3 <- mcPartition(is.null, x_, paropts_)

		length_unique <- length(unique(list(res_0, res_1, res_2)))
		length_unique == 1
	}, opts = list(time = 5)
)
