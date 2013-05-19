
context ("test that mcPartition is well behaved for normal cases")

test_that ("", {
	
	expect_equal(
		mcPartition (
			function (x) x %% 2,	
			list(1,2,3,4,5),
			paropts = list(mc.cores = 6)
		),
		list (
			list(1, 3, 5),	
			list(2, 4)
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
	
	expect_equal (
		mcPartition(
			function (x) x %% 2, 
			1:10,
			list(mc.cores = 12) ), 	
			list (
				c(1,3,5,7,9),
				c(2,4,6,8,10)
	) )
	
	expect_equal (
		mcPartition (
			function (x) unlist(x) == 0,
			list (
				list (1), list (0), list (1), list (0)	
			) ),
		list (
			list ( list (0), list (0) ),	
			list ( list (1), list (1)) )	
	)
	
})

