
context('quickcheck functions where possible')

test_that('', {

	forall(
		list(x_ = lists_with_null(), paropts_ = random_paropts),
		function (x_, paropts_) {
			res_1 <- mcFilter(Negate(is.null), x_, paropts_)
			res_2 <- denullify(x_)
	})

})
