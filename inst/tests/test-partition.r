
context ("test that mcPartition is well behaved for normal cases")

test_that ("", {
	
	expect_equal (
		mcPartition (
			function (n) n %% 2,
			seq_len(10)	),
		list (
			c (1,3,5,7,9),	
			c (2,4,6,8,10) ) )	
	
	expect_equal(
		mcPartition (
			function (x) FALSE,	
			1:10,
			paropts = list(mc.cores = 6)
		),
		list (
			integer(0),	
			seq_len(10)
		))
	
	expect_equal (
		mcPartition (
			function (x) x,
			list (
				list (1), list (2), list (3)	
			)),	
		list(
			list(),
			list(
				list(1),
				list(2),
				list(3)
		))
	)
	
})

